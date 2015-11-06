/*
 * Copyright 2015 The SIRIS Project
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

package simx.core.worldinterface.base

import simx.core.entity.typeconversion.TypeInfo._
import simx.core.svaractor.SVarActor
import simx.core.worldinterface.WorldInterfaceActor

import scala.reflect.ClassTag

/**
 * Created by martin 
 * on 21/04/15.
 */
private[worldinterface] trait WorldInterfaceHandlingBase extends SVarActor {

  /**
   *
   * this one is a bit advanced, so here is what happens:
   * the function takes two parameters, one function that instanciates the message to be send and the handler which
   * shall be executed when an answer to that message is sent.
   * Initially an ID is generated and stored to identify the message which is sent. Then a single-use-handler is
   * installed, which uses that generated id. The installed handler simply calls the function "handler" which is
   * provided as a parameter, applying the value returned by the worldInterfaceActor.
   * Finally a message is sent to the worldInterfaceActor, which contains an handler (executed by the
   * worldInterfaceActor), that sends the id, value tuple back, causing the invocation of the installed handler
   *
   */
  protected def nonBlockingHandler[T](msg : Any, handler : T => Any)
                                     (implicit actorContext : SVarActor, m : ClassTag[T], t : DataTag[T]) {
    actorContext.ask[T](WorldInterfaceActor.self, msg)(handler(_) : Unit)
  }
}
