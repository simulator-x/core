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
import simx.core.svaractor.TimedRingBuffer.{Now, Unbuffered, BufferMode}
import simx.core.worldinterface.WorldInterfaceHandling


abstract class Relation private[unifiedaccess](subj : Entity, description : RelationDescription[_ <: Entity, _ <: Entity], obj :Entity){
  def remove()(implicit context : WorldInterfaceHandling with EntityUpdateHandling)
  def getSubject : Entity
  def getObject : Entity
}

final case class TypedRelation[S <: Entity : DataTag, O <: Entity : DataTag, X <: StateParticleAccess : DataTag] private[unifiedaccess](subj : S, description : RelationDescription[S, O], obj : O,
                                                                                                                                        update : (X => ((X#SelfType => Any) => Unit)) => Unit )
  extends Relation(subj, description, obj){
  def publish(bufferMode : BufferMode = Unbuffered)(implicit context : WorldInterfaceHandling with EntityUpdateHandling){
    val sval = description(this)
    update{ toSet => handler =>
      (if (toSet == obj) subj else obj).set(sval, Now, bufferMode)((_ : Any) => toSet.set(sval, Now, bufferMode)(handler))
    }
    context.setRelation(sval)
  }

  def remove()(implicit context : WorldInterfaceHandling with EntityUpdateHandling){
    update { toSet => handler =>
      (if (toSet == obj) subj else obj).remove(description, (_: Any) => toSet.remove(description, handler)(context))
    }
    context removeRelation description(this)
  }

  override def getSubject: S =
    subj

  override def getObject: O =
    obj

  override def toString: String =
    subj.getSimpleName + " " + description.semantics.value.toString + " " + obj.getSimpleName
}


