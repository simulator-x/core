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

import java.util.UUID
import simx.core.component._
import simx.core.svaractor.{SingletonActor, SVarActor}
import simx.core.entity.component._
import simx.core.ontology
import simx.core.entity.Entity
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

object RemoteActor extends SingletonActor(new RemoteActor, "simx-remote-actor-" + UUID.randomUUID()){
  def checkRemoting(){
    if (SVarActor.getHostname.isEmpty)
      throw new Exception("Remoting enabled. Pass \"enableRemoting\" to your application to enable remoting.")
  }
}

protected case class RegisterService(name : String, ref : SVarActor.Ref)
protected case class NewRegistration(name : String, path : String)
protected case class ObserveRegistrations(name : String, observer : SVarActor.Ref)
protected case class Started(idTuple : UUID, path : String)
protected case class Instantiate(aspect : ComponentAspect[_ <: Component], id : UUID, sender : SVarActor.Ref)
case class Start[T <: Component](serviceType : String, aspect : RemoteComponentAspect[T]){
  val id = UUID.randomUUID()
}

protected class RemoteActor extends SVarActor with EntityCreationHandling with EntityUpdateHandling{
  protected var registerObservers = Map[String, Set[SVarActor.Ref]]()
  protected var openRequests = Map[UUID, SVarActor.Ref => Any]()
  protected var targets = Map[String, Start[_ <: Component]]()
  protected var nodes   = Map[String, String]()

  override protected def startUp(){
    JmDNSActor.observeServices
  }

  protected def removeFromLocalRep(e : Entity){}

  private def initiateCreation[T <: Component](path : String, msg : Start[T]){
    context.actorSelection(path) ! Instantiate(msg.aspect.base, msg.id, self)
    targets = targets - path
  }

  private def serviceMatch(srv : String, toCheck : String) : Boolean =
    srv.matches(toCheck + "( \\([0-9]+\\))?")

  addHandler[RegisterService]{ msg =>
    JmDNSActor.register(msg.name)(msg.ref)
  }

  addHandler[Registered]{ msg =>
    nodes = nodes.updated(msg.serviceType, msg.path)
    registerObservers.foreach {
      kv => if (serviceMatch(msg.serviceType, kv._1)) kv._2.foreach( _ ! NewRegistration(kv._1, msg.path) )
    }
    if (targets.contains(msg.serviceType))
      initiateCreation(msg.path, targets(msg.serviceType))
  }

  addHandler[ObserveRegistrations]{ msg =>
    nodes.filter(n => serviceMatch(msg.name, n._1)) foreach { t => msg.observer ! NewRegistration(t._1, t._2) }
    registerObservers = registerObservers.updated(msg.name, registerObservers.getOrElse(msg.name, Set()) + sender)
  }

  addHandler[Start[_ <: Component]]{ msg =>
    targets = targets.updated(msg.serviceType, msg)
    if (nodes.contains(msg.serviceType))
      initiateCreation(nodes(msg.serviceType), msg)
    openRequests = openRequests.updated(msg.id, provideAnswer[SVarActor.Ref])
    DelayedAnswer
  }

  addHandler[Instantiate]{
    msg => ComponentEntityDescription(msg.aspect).realize{
      componentEntity => componentEntity.get(ontology.types.Component).head{
        component => msg.sender ! Started(msg.id, getAddressOf(component))
      }
    }
  }

  addHandler[Started]{ msg =>
    ask[ComponentIdentity](context.actorSelection(msg.path), IdentifyComponent(msg.idTuple)){
      identity => openRequests.get(identity.id).collect{ case reply => reply apply identity.ref }
    }
  }
}
