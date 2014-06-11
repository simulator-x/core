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

import simx.core.SimXApplication
import simx.core.component.Component
import simx.core.svaractor.SVarActor
import scala.reflect._
import akka.actor.Props

/**
  * User: dwiebusch
  * Date: 14.11.13
  * Time: 14:24
  */

trait RemoteCreation extends SimXApplication{
   protected def registerComponentCreationSupport[T <: Component : ClassTag](name : String){
     RemoteActor.checkRemoting()
     RemoteActor.self ! RegisterService(name, RemoteActor.self)
   }
 }