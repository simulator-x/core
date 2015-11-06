/*
 * Copyright 2014 The SIRIS Project
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

package simx.core.svaractor.unifiedaccess

import simx.core.entity.Entity
import simx.core.entity.typeconversion.TypeInfo.DataTag
import simx.core.svaractor.TimedRingBuffer
import simx.core.svaractor.TimedRingBuffer.{Now, Unbuffered, BufferMode}
import simx.core.worldinterface.WorldInterfaceHandling


abstract class Relation private[unifiedaccess](subj : Entity, description : RelationDescription[_ <: Entity, _ <: Entity], obj :Entity) extends Serializable{
  protected type removeType <: StateParticleAccess
  def remove(update : removeType#SelfType => Any)(implicit context : WorldInterfaceHandling with EntityUpdateHandling)
  def getSubject : Entity
  def getObject : Entity
}

final case class TypedRelation[S <: Entity : DataTag, O <: Entity : DataTag, X <: StateParticleAccess : DataTag] private[unifiedaccess](subj : S, description : RelationDescription[S, O], obj : O, x : X)
  extends Relation(subj, description, obj) with Serializable{

  protected type removeType = X

  def publish(handler : X#SelfType => Any, bufferMode : BufferMode = TimedRingBuffer.defaultMode)(implicit context : WorldInterfaceHandling with EntityUpdateHandling){
    val sval = description(this)
    (if (x == obj) subj else obj).set(sval, Now, bufferMode)((_ : Any) => x.set(sval, Now, bufferMode)(handler))
    context.setRelation(sval)
  }

  def remove(handler : (X#SelfType => Any))(implicit context : WorldInterfaceHandling with EntityUpdateHandling){
    (if (x == obj) subj else obj).remove(description, (_: Any) => x.remove(description, handler)(context))
    context removeRelation description(this)
  }

  override def getSubject: S =
    subj

  override def getObject: O =
    obj

  override def toString: String =
    subj.getSimpleName + " " + description.semantics.toString + " " + obj.getSimpleName
}


