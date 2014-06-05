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

package simx.core.dynamics

import simx.core.entity.Entity
import simx.core.entity.description.SVal

/**
 * Created by IntelliJ IDEA.
 * User: dwiebusch
 * Date: 24.02.12
 * Time: 11:53
 * To change this template use File | Settings | File Templates.
 */

case class Prerequisite[T]( target : Entity, value : SVal[T] ){
  def metBy( e : Effect[_]) =
    e.changedValue.equals(value)

  def asEffect =
    Effect(target, value)

  def bind(entity : Entity) =
    Prerequisite(entity, value)

  def metBy( es : Set[Effect[_]]) : Boolean = {
    for( e <- es )
      if (metBy(e))
        return true
    false
  }
}
