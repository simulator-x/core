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

import simx.core.svaractor.TimedRingBuffer.Time
import simx.core.svaractor.{SVarActor, SVar}

/**
 * Created by dwiebusch on 14.03.14
 */

trait OuterType[T]{
  def asOuterType : T
}

trait EntityBase[T <: EntityBase[T]] extends EntityRelationAccess with OuterType[T]{
  type SelfType = T

  override def asOuterType: T = asSelfType

  protected var selfSVar : SVar[Option[SelfType]]

  protected def setSelf(value : Option[SelfType], at : Time)(implicit actorContext : SVarActor) =
    if (selfSVar.set(value, at, forceUpdate = true))
      value
    else
      throw new Exception("Failed to set self SVar")
}
