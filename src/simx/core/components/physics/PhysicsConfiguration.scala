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

import simx.core.ontology.Symbols
import simx.core.entity.description.SValSet
import simplex3d.math.floatx.ConstVec3f

//Global Types
import simx.core.ontology.{types => gt}

/*
* User: martin
* Date: 6/10/11
* Time: 10:23 AM
*/

/**
 *  Used to configure a PhysicsComponent
 *
 * @param simulationSpeed A multiplyer to deltaT. If this value is greater than 1.0f the simulation runs faster and vice versa.
 * @param gravity         The global gravity of the simulation.
 */
case class PhysicsConfiguration(gravity: ConstVec3f, simulationSpeed: Float = 1.0f) {

  def toConfigurationParams: SValSet =
    new SValSet(gt.Gravity(gravity), gt.SimulationSpeed(simulationSpeed))

  def targetComponentType = Symbols.physics
}