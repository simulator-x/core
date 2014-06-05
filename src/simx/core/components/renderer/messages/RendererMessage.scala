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

package simx.core.components.renderer.messages

import simx.core.svaractor.{SVarActor, SimXMessage}
import simx.core.components.renderer.setup.DisplaySetupDesc
import scala.annotation.meta.param


/**
 * This is the base class of all messages for the renderer.
 *
 * @author Stephan Rehfeld
 */
abstract class RendererMessage()(implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage with Serializable

/**
 * This class sends a configuration to a rendering connector.
 *
 * @author Stephan Rehfeld
 *
 * @param displaySetup The display setup description that should be interpreted by the renderer to set up the windows.
 * @param effectsConfiguration The [[simx.core.components.renderer.messages.EffectsConfiguration]].
 *
 */
case class ConfigureRenderer( displaySetup: DisplaySetupDesc, effectsConfiguration : EffectsConfiguration )
                            (implicit @(transient @param) actor : SVarActor.Ref) extends RendererMessage {
  require( displaySetup != null, "The parameter 'displaySetup' must not be 'null'!")
  require( effectsConfiguration != null, "The parameter 'effectsConfiguration' must not be 'null'!")
}

/**
 * This class represents a effects configuration. You can control if a effect is enabled and the quality of the effect.
 *
 * @author Stephan Rehfeld
 *
 * @param shadowQuality The quality of the shadows. "none", "low", "middle" or "high"
 * @param mirrorQuality The quality of the mirrors. "none", "low", "middle" or "high"
 */
case class EffectsConfiguration( shadowQuality : String, mirrorQuality : String ) {
  require( shadowQuality != null, "The parameter 'shadowQuality' must not be 'null'!")
  require( mirrorQuality != null, "The parameter 'mirrorQuality' must not be 'null'!")
}

/**
 * @author Alexander Strehler
 * switching left and right eye
 */
case class SwitchEyes()(implicit @(transient @param) actor : SVarActor.Ref) extends RendererMessage
