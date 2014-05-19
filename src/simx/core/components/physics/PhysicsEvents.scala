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

import simx.core.worldinterface.eventhandling.EventDescription
import simx.core.ontology.Symbols


/*
* Created by IntelliJ IDEA.
* User: martin
* Date: 6/8/11
* Time: 10:31 AM
*/
object PhysicsEvents {

  /**
   * An event of this type should
   * contain no additional data
   */
  val collision = new EventDescription( Symbols.collision )

  /**
   * An event of this type should
   * contain a simx.ontology.types.Impulse cvar
   */
  val impulse = new EventDescription( Symbols.impulse )

  /**
   * An event of this type should
   * contain a simx.ontology.types.Impulse cvar
   */
  val torqueImpulse = new EventDescription( Symbols.torqueImpulse )
}