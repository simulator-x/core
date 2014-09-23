/*
 * Copyright 2012 The SIRIS Project
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

package simx.core.components.physics

import simx.core.component.Component
import simx.core.worldinterface.eventhandling.{EventHandler, EventProvider}
import simx.core.ontology.Symbols
import simx.core.entity.Entity
import simx.core.entity.description.SVal
import scala.reflect.ClassTag
import simx.core.entity.component.ComponentAspect
import simx.core.entity.typeconversion.TypeInfo

//Global Types
import simx.core.ontology.{types => gt}

/*
* Created by IntelliJ IDEA.
* User: martin
* Date: 6/10/11
* Time: 10:30 AM
*/

abstract class PhysicsComponentAspect[T <: Component : ClassTag](name : Symbol, args : Seq[Any])
  extends ComponentAspect[T](PhysicsComponent.componentType, name, args)

object PhysicsComponent{
  final def componentType = Symbols.physics
}

/**
 *  Base trait for all physics components
 */
abstract class PhysicsComponent(name : Symbol) extends Component(name, PhysicsComponent.componentType) with EventProvider with EventHandler {
  /**
   *  Sets the transformation of an entity.
   *
   * @param e The entity
   * @param t The transformation
   *
   * @see   [[simx.core.components.physics.PhysicsMessage]]
   */
  def handleSetTransformation(e: Entity, t: SVal[gt.Transformation.dataType,TypeInfo[gt.Transformation.dataType,gt.Transformation.dataType]])
  addHandler[SetTransformation]{msg => handleSetTransformation(msg.e, gt.Transformation(msg.t))}

  /**
   *  Applies an impules to an entity
   *
   * @param e The entity
   * @param i The impulse
   *
   * @see   [[simx.core.components.physics.PhysicsMessage]]
   */
  def handleApplyImpulse(e: Entity, i: SVal[gt.Impulse.dataType,TypeInfo[gt.Impulse.dataType,gt.Impulse.dataType]])
  addHandler[ApplyImpulse]{msg => handleApplyImpulse(msg.e, gt.Impulse(msg.i))}

  /**
   *  Applies an impules to an entity
   *
   * @param e The entity
   * @param i The impulse
   *
   * @see   [[simx.core.components.physics.PhysicsMessage]]
   */
  def handleApplyTorqueImpulse(e: Entity, i: SVal[gt.Impulse.dataType,TypeInfo[gt.Impulse.dataType,gt.Impulse.dataType]])
  addHandler[ApplyTorqueImpulse]{msg => handleApplyTorqueImpulse(msg.e, gt.Impulse(msg.i))}

  /**
   *  Sets the linear velocity of an entity.
   *
   * @param e The entity
   * @param v The linear velocity
   *
   * @see   [[simx.core.components.physics.PhysicsMessage]]
   */
  def handleSetLinearVelocity(e: Entity, v: SVal[gt.Velocity.dataType,TypeInfo[gt.Velocity.dataType,gt.Velocity.dataType]])
  addHandler[SetLinearVelocity]{msg => handleSetLinearVelocity(msg.e, gt.Velocity(msg.v))}

  /**
   *  Sets the angular velocity of an entity.
   *
   * @param e The entity
   * @param v The angular velocity
   *
   * @see   [[simx.core.components.physics.PhysicsMessage]]
   */
  def handleSetAngularVelocity(e: Entity, v: SVal[gt.Velocity.dataType,TypeInfo[gt.Velocity.dataType,gt.Velocity.dataType]])
  addHandler[SetAngularVelocity]{msg => handleSetAngularVelocity(msg.e, gt.Velocity(msg.v))}

  /**
   *  Sets the gravity of an entity.
   *
   * @param e The entity
   * @param g The gravity
   *
   * @see   [[simx.core.components.physics.PhysicsMessage]]
   */
  def handleSetGravity(e: Entity, g: SVal[gt.Gravity.dataType,TypeInfo[gt.Gravity.dataType,gt.Gravity.dataType]])
  addHandler[SetGravity]{msg => handleSetGravity(msg.e, gt.Gravity(msg.g))}

  /**
   *  Detaches e from the physical simulation,
   *        until it is reattached using the handleAttachEntity message.
   *
   * @param e The entity
   *
   * @see   [[simx.core.components.physics.PhysicsMessage]]
   */
  def handleDetachEntity(e: Entity)
  addHandler[DetachEntity]{msg => handleDetachEntity(msg.e)}

  /**
   *  Reattachs an entity that was previously
   *        detached from physical simulation using handleDetachEntity
   *
   * @param e The entity
   *
   * @see   [[simx.core.components.physics.PhysicsMessage]]
   */
  def handleAttachEntity(e: Entity)
  addHandler[AttachEntity]{msg => handleAttachEntity(msg.e)}

  /**
   * Handle all messages.
   */
  addHandler[BunchOfPhysicsMessages]{msg => msg.msgs.foreach( applyHandlers(_) )}

}