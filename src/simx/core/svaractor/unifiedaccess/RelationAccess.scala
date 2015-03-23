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
import simx.core.svaractor.TimedRingBuffer.{Unbuffered, BufferMode}
import simx.core.worldinterface.WorldInterfaceHandling

/**
 * Created by dennis on 24.04.14.
 *
 */
trait RelationAccess extends UnifiedAccess{
  protected implicit def toRightUnknownTuple[V](t : (V, Unknown)) : RightUnknownTuple[V] =
    RightUnknownTuple(t._1)

  final def set(desc : PartialRelation[SelfType, _ <: Entity, _ <: EntityBase[_]])
               (implicit actorContext : WorldInterfaceHandling with EntityUpdateHandling){
    set(desc, Unbuffered)
  }

  final def set(desc : PartialRelation[SelfType, _ <: Entity , _ <: EntityBase[_]], bufferMode : BufferMode)
               (implicit actorContext : WorldInterfaceHandling with EntityUpdateHandling)
  {
    desc.complete[SelfType](asSelfType).publish( (_ : SelfType ) => {}, bufferMode)
  }

  final def set(desc : PartialRelation[SelfType, _ <: Entity , _ <: EntityBase[_]],
                handler : SelfType => Any)
               (implicit actorContext : WorldInterfaceHandling with EntityUpdateHandling)
  {
    set(desc, handler, Unbuffered)
  }

  final def set(desc : PartialRelation[SelfType, _ <: Entity , _ <: EntityBase[_]],
                handler : SelfType => Any, bufferMode : BufferMode)
               (implicit actorContext : WorldInterfaceHandling with EntityUpdateHandling)
  {
    desc.complete[SelfType](asSelfType).publish(handler, bufferMode)
  }

  final def remove(desc : PartialRelation[SelfType , _ <: Entity , _ <: EntityBase[_]])
                  (implicit actorContext : WorldInterfaceHandling with EntityUpdateHandling)
  {
    remove(desc, (_ : SelfType ) => {} )
  }

  final def remove(desc : PartialRelation[SelfType , _ <: Entity , _ <: EntityBase[_]],
                   handler : SelfType => Any)
                  (implicit actorContext : WorldInterfaceHandling with EntityUpdateHandling)
  {
    desc.complete[SelfType](asSelfType).remove(handler)
  }
}