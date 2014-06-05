/*
 * Copyright 2013 The SIRIS Project
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

import java.util.UUID
import simx.core.ontology
import simx.core.svaractor.SVarActor
import simx.core.ontology.GroundedSymbol

/**
 * User: dwiebusch
 * Date: 20.11.13
 * Time: 19:14
 */
trait ComponentIdentification extends SVarActor{
  /**
   * the components name (used for lookup functionality)
   * @return the components name
   */
  def componentName : Symbol

  /**
   * the components type (used for lookup functionality)
   * @return the components type
   */
  def componentType : GroundedSymbol

  addHandler[IdentifyComponent]{
    msg => ComponentIdentity(componentType, componentName, self, msg.id)
  }
}

case class IdentifyComponent(id : UUID)

case class ComponentIdentity( cType : ontology.GroundedSymbol, cName : Symbol, ref : SVarActor.Ref, id : UUID )
