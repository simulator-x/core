/*
 * Copyright 2013 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the
 * HCI Group at the University of Würzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.core.component.remote

import java.net.InetAddress
import javax.jmdns.{ServiceListener, ServiceEvent, ServiceInfo, JmDNS}
import simx.core.svaractor.{SingletonActor, SVarActor}


/**
 * User: dwiebusch
 * Date: 13.11.13
 * Time: 11:22
 */
object JmDNSActor extends SingletonActor(new JmDNSActor, "jmdns-service"){
  sealed case class RegisterService(serviceName : String, ref : SVarActor.Ref)
  sealed case class UnRegisterService(serviceName : String)
  sealed case class LookupService(serviceName : String)
  sealed case class AddObserver(observer : SVarActor.Ref)

  val serviceType =
    "_simx._tcp.local."

  def register(name : String)(implicit ref : SVarActor.Ref){
    self ! RegisterService( name, ref )
  }

  def observeServices(implicit observer : SVarActor.Ref){
    self ! AddObserver(observer)
  }

  def unregister(ref : SVarActor.Ref){
    self ! UnRegisterService(ref.path.toString)
  }
}

case class Registered(serviceType : String, path : String, port : Int)
case class UnRegistered(path : String)

private case class InnerRegister(info : ServiceInfo)
private case class InnerUnRegister(info : ServiceInfo)

protected class JmDNSActor extends SVarActor{
  private var browser : Option[SVarActor.Ref] = None

  protected var observers = Set[SVarActor.Ref]()

  protected var localServices = Map[String, ServiceInfo]()
  protected var registeredServices = Map[String, List[List[(InetAddress, Int)]]]()

  protected type requestType = List[(String, List[(InetAddress, Int)])]
  protected var openRequests = Map[String,  requestType => Any]()
  private var queuedMessages = List[InnerRegister]()

  private def makeRegMsg(name : String, info : ServiceInfo) : Registered =
    Registered(name, info.getNiceTextString, info.getPort)

  override protected def startUp(){
    createActor(new Browser(self,
      SVarActor.getHostname.collect{ case hn => InetAddress.getByName(hn)}.getOrElse(InetAddress.getLocalHost))
    ){ b =>
      browser = Some(b)
      queuedMessages.foreach(b ! _)
      queuedMessages = Nil
    }()
  }

  addHandler[JmDNSActor.AddObserver]{
    msg => observers = observers + msg.observer
  }

  addHandler[InnerRegister]{ msg =>
    val info = msg.info
    val name = info.getName
    val newList = info.getInetAddresses.map( _ -> info.getPort ).toList
    println("found service " + msg.info)
    observers.foreach{ _ ! makeRegMsg(name, info) }
    registeredServices = registeredServices.updated(name, newList :: registeredServices.getOrElse(name, Nil))
    openRequests.filter( kv => name.matches(kv._1) ).foreach{ _._2.apply(List(name -> newList)) }
    openRequests = openRequests.filterNot( kv => name.matches(kv._1) )
  }

  addHandler[InnerUnRegister]{
    msg => registeredServices = registeredServices - msg.info.getName
  }

  addHandler[JmDNSActor.RegisterService]{ msg =>
    if (localServices.contains(msg.serviceName))
      println("service with name " + msg.serviceName + " was already registered")
    else if (getPort.isEmpty)
      println("Actor system is not remote-enabled, service " + msg.serviceName + " not registered")
    else if (getPort.isDefined){
      val newService =
        ServiceInfo.create(JmDNSActor.serviceType, msg.serviceName, getPort.get, 0, 0, true, getAddressOf(msg.ref))
      browser match {
        case Some(b) =>
          localServices = localServices.updated(msg.serviceName, newService)
          b ! InnerRegister(newService)
        case None =>
          localServices = localServices.updated(msg.serviceName, newService)
          queuedMessages ::= InnerRegister(newService)
      }
    }
  }

  addHandler[JmDNSActor.UnRegisterService]{
    msg => browser.collect{
      case b if localServices.contains(msg.serviceName) =>
        b ! InnerUnRegister(localServices(msg.serviceName))
    }
  }

  addHandler[JmDNSActor.LookupService]{
    msg =>
      val matched = registeredServices.filter( _._1.matches(msg.serviceName) )
      if (matched.nonEmpty)
        matched.foldLeft(List[(String, List[(InetAddress, Int)])]()){
          (list, kv) => (kv._1 -> kv._2.flatten) :: list
        }
      else {
        openRequests = openRequests.updated(msg.serviceName, provideAnswer[requestType])
        DelayedAnswer
      }
  }
}

private class Browser(outer : SVarActor.Ref, host : InetAddress = InetAddress.getLocalHost)
  extends SVarActor with ServiceListener
{
  private case class InnerAdd(event : ServiceEvent)
  private val browser = JmDNS.create(host)

  override protected def startUp(){
    browser.registerServiceType(JmDNSActor.serviceType)
    browser.addServiceListener(JmDNSActor.serviceType, this)
    update()
  }

  override def postStop(){
    browser.unregisterAllServices()
    browser.close()
  }

  protected def update(){
    browser.list(JmDNSActor.serviceType, 100)
    addJobIn(100)(update())
  }

  def serviceAdded(event: ServiceEvent){
    self ! InnerAdd(event)
  }

  addHandler[InnerAdd]{
    msg => browser.requestServiceInfo(msg.event.getType, msg.event.getName)
  }

  def serviceResolved(event: ServiceEvent){
    outer ! InnerRegister(event.getInfo)
  }

  def serviceRemoved(event: ServiceEvent){
    outer ! InnerUnRegister(event.getInfo)
  }

  addHandler[InnerRegister]{
    msg =>
      println("publishing service " + msg.info)
      browser.registerService(msg.info)
  }

  addHandler[InnerUnRegister]{
    msg => browser.unregisterService(msg.info)
  }
}
