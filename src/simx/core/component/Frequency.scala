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

package simx.core.component

/**
 * This is the common base class for all run frequency descriptions of a component.
 * @author Stephan Rehfeld
 */
class Frequency

/**
 * The component runs unbound with maximum frequency.
 *
 * @author Stephan Rehfeld
 */
case class Unbound() extends Frequency

/**
 * The component is triggered in the given frequency. If an simulation frame needs longer
 * the performSimulation function is called immediately.
 *
 * @author Stephan Rehfeld
 *
 * @param hz The frequency. A value larger than 0.
 */
case class Hard( hz : Float ) extends Frequency  {
  require( hz > 0.0f, "The parameter 'hz' must be larger than 0!" )
}

/**
 * The component is triggered in the given frequency. If an simulation frame needs longer
 * the component is triggered immediately.
 *
 * @author Stephan Rehfeld
 *
 * @param hz The frequency. A value larger than 0.
 */
case class Soft( hz : Float ) extends Frequency  {
  require( hz > 0.0f, "The parameter 'hz' must be larger than 0!" )
}

/**
 * The component is triggered in the given frequency. If an simulation frame needs longer
 * frames are skipped.
 *
 * @author Stephan Rehfeld
 *
 * @param hz The frequency. A value larger than 0.
 */
case class Weak( hz : Float ) extends Frequency  {
  require( hz > 0.0f, "The parameter 'hz' must be larger than 0!" )
}

/**
 * The component is triggered by another component or manually by sending a PerformSimulationStep
 * message.
 *
 * @author Stephan Rehfeld
 */
case class Triggered() extends Frequency

/**
 * The component is passive. It's working by processing and triggering events but not by an regular
 * simulation message. [[simx.core.component.PerformSimulationStep]] messages are ignored.
 *
 * @author Stephan Rehfeld
 *
 */
case class Passive() extends Frequency