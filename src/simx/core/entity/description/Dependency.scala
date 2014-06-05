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

package simx.core.entity.description

import simx.core.entity.typeconversion.ConvertibleTrait
import collection.mutable

/**
 * User: dwiebusch
 * Date: 06.05.11
 * Time: 14:37
 */

case class Providing( objects : ConvertibleTrait[_]* )

case class Requiring( objects : ConvertibleTrait[_]* )

case class Dependencies( providings : Providing, requirings : Requiring )

object Dependencies{
  def apply( p : Providing ) : Dependencies =
    Dependencies(p, Requiring())

  def apply( r : Requiring ) : Dependencies =
    Dependencies(Providing(), r)
}

object DependenciesSet {
  //This needs no include. The compiler looks by default in companion object
  implicit def toSet(dSet: DependenciesSet): Set[Dependencies] = dSet.toSet
}
class DependenciesSet extends mutable.HashSet[Dependencies] {
  def += (r: Requiring): mutable.HashSet[Dependencies] = this.+=(Dependencies(r))
  def += (p: Providing): mutable.HashSet[Dependencies] = this.+=(Dependencies(p))
  def += (providings : Providing, requirings : Requiring): mutable.HashSet[Dependencies] =
    this.+=(Dependencies(providings, requirings))
}