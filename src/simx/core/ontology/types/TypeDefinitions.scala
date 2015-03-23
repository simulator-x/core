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

/*
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 5/3/11
 * Time: 10:04 AM
 */
package simx.core.ontology.types

import simx.core.ontology.{SValDescription, SpecificDescription, Symbols}
import simx.core.svaractor.semantictrait.base.Base

object TypeDefinitions {
  type Enum = scala.Enumeration#Value
}

object Mappings{
  val keyMap = Map(
    32 -> Key_Space,
    149 -> Key_Left,
    150 -> Key_Up,
    151 -> Key_Right,
    152 -> Key_Down,
    48 -> Key_0,
    49 -> Key_1,
    50 -> Key_2,
    51 -> Key_3,
    52 -> Key_4,
    53 -> Key_5,
    54 -> Key_6,
    55 -> Key_7,
    56 -> Key_8,
    57 -> Key_9,
    65 -> Key_a,
    66 -> Key_b,
    67 -> Key_c,
    68 -> Key_d,
    69 -> Key_e,
    70 -> Key_f,
    71 -> Key_g,
    72 -> Key_h,
    73 -> Key_i,
    74 -> Key_j,
    75 -> Key_k,
    76 -> Key_l,
    77 -> Key_m,
    78 -> Key_n,
    79 -> Key_o,
    80 -> Key_p,
    81 -> Key_q,
    82 -> Key_r,
    83 -> Key_s,
    84 -> Key_t,
    85 -> Key_u,
    86 -> Key_v,
    87 -> Key_w,
    88 -> Key_x,
    89 -> Key_y,
    90 -> Key_z )
}

case class SoundProperties(gain : scala.Float = 0.5f, loop: scala.Boolean = false, pitch: scala.Float = 1.0f, max_distance : scala.Float = Float.MaxValue, ref_distance : Float = 1.0f)

object DefaultEnum extends scala.Enumeration {val Foo = Value("Foo") : TypeDefinitions.Enum}
object EntityDescription extends SValDescription[SpecificDescription, SpecificDescription,Base, Symbols.entityDescription.SymbolType](
  NullType as Symbols.entityDescription withType classOf[simx.core.ontology.SpecificDescription]
)