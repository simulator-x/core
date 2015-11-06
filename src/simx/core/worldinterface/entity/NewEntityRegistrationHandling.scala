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

package simx.core.worldinterface.entity

import java.util.UUID

import simx.core.entity.Entity
import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling
import simx.core.worldinterface._
import simx.core.worldinterface.base.WorldInterfaceHandlingBase
import simx.core.worldinterface.entity.filter.EntityFilter


/**
 * Created by martin 
 * on 21/04/15.
 */
private[worldinterface] trait NewEntityRegistrationHandling extends WorldInterfaceHandlingBase {

  private type CreationMessageHandler = (Entity) => Any
  private var creationMessageHandlers = Map[UUID, CreationMessageHandler]()

  override def preStart(): Unit = {
    super.preStart()
    addHandler[CreationMessageNew]{msg =>
      creationMessageHandlers.get(msg.reference).collect{case h => h.apply(msg.e)}
    }
  }

  //TODO Uses the same message as the old entity registration for now
  //  final protected  def registerEntity(e : Entity) {
  //    WorldInterfaceActor ! EntityRegisterRequest(e)
  //  }

  //TODO Uses the same message as the old entity registration for now
  //  final protected def unregisterEntity(e : Entity) {
  //    WorldInterfaceActor ! EntityUnregisterRequest(e)
  //  }

  /**
   *  Applies the passed handler for the next entity that will satisfy the passed filter.
   */
  final protected def onNextEntityAppearance(filter : EntityFilter)(f : Entity => Any) {
    nonBlockingHandler[Entity](OnNextEntityAppearance(filter), f)
  }

  /**
   *  Applies the passed handler for all entities that will satisfy the passed filter.
   */
  final protected def onEntityAppearance(filter : EntityFilter)(f : Entity => Any) {
    val request = AddOnEntityAppearanceListener( self, filter )
    WorldInterfaceActor ! request
    creationMessageHandlers = creationMessageHandlers.updated(request.reference, f)
  }

  /**
   *  Applies the passed handler for all entities that have been registered so far and satisfy the passed filter.
   */
  final protected def requestRegisteredEntities(filter : EntityFilter)(f: Set[Entity] => Any) {
    //def handler(entities: Set[Entity]) = entities.foreach(f)
    nonBlockingHandler[Set[Entity]](RequestEntities(filter), f)
  }

  /**
   *  Applies the passed handler for one entity that satisfies the passed filter.
   *  If no such entity has been registered so far,
   *  the passed handler is applied once for the next entity will satisfy the passed filter.
   */
  final protected def onOneEntityAppearance(filter : EntityFilter)(f : Entity => Any) {
    nonBlockingHandler[Entity](OnOneEntityAppearance(filter), f)
  }

  /**
   *  Convenience method.
   *  Applies the passed handler for all entities that have been registered so far and satisfy the passed filter.
   *  In addition, the passed handler is applied for all entities that will satisfy the passed filter.
   */
  final protected def handleEntityRegistration(filter : EntityFilter)(f : Entity => Any){
    requestRegisteredEntities(filter)(_.foreach(f))
    onEntityAppearance(filter)(f)
  }
}
