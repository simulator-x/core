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

package simx.core.ontology.functions

import simx.core.ontology.types._

/**
 * Created by martin on 27/02/15.
 */



object DefaultImplementations {

  //implicit object defaultImplementations extends Functions

  def positionOf(param1: Entity): Position = ???

  def booleanOf(param1: Position, param2: Entity): Boolean = ???

  def lengthOf(param1: Position, param2: Position): Length = ???

  def lengthOf(p1: Vector3): Length =
    Length(simplex3d.math.float.functions.length(p1.value))

  def +(p1: Length, p2: Length): Length =
    Length(p1.value + p2.value)

  def approximates(p1: Time, p2: Time): Boolean =
    Boolean(300L > (if (p1.value > p2.value) p1.value - p2.value else p2.value - p1.value))

  def equals(p1: Time, p2: Time): Boolean =
    Boolean(p1.value == p2.value)

  /**
   * Calculates the angle between two vectors.
   */
  def angleBetween(p1: Vector3, p2: Vector3): Angle =
    Angle(math.acos(simplex3d.math.float.functions.dot(p1.value, p2.value)).toFloat)

  def -(p1: Length, p2: Length): Length =
    Length(p1.value - p2.value)
}