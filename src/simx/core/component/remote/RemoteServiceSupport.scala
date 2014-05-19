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

import simx.core.svaractor.SVarActor
import akka.actor.{Identify, ActorIdentity}

/**
 * User: dwiebusch
 * Date: 16.11.13
 * Time: 17:33
 */
trait RemoteServiceSupport extends SVarActor{
  private var publishHandlers = Map[String, SVarActor.Ref => Any]()

  protected def observeRegistrations(name : String)(handler : SVarActor.Ref => Any){
    publishHandlers = publishHandlers.updated(name, handler)
    RemoteActor.self ! ObserveRegistrations(name, self)
  }

  protected def publishActor( name : String, ref : SVarActor.Ref ){
    RemoteActor.self ! RegisterService(name, ref)
  }

  addHandler[NewRegistration]{
    msg => context.actorSelection(msg.path) ! Identify(ServiceName(msg.name))
  }

  addHandler[ActorIdentity]{
    case ActorIdentity(ServiceName(name), Some(ref)) => publishHandlers.get(name).foreach(_ apply ref)
    case ActorIdentity(ServiceName(name), None) => println("remote actor "+ name +
      " sadly died before we got to know him better...")
  }
}

private case class ServiceName(name : String)
