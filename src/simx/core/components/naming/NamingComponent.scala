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

package simx.core.components.naming

import simx.core.component.{SingletonComponent, Component}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.component.EntityConfigLayer
import simx.core.entity.Entity
import simx.core.entity.description._
import simx.core.ontology.types.Name
import simx.core.ontology.{GroundedSymbol, Symbols}

/**
 * User: dwiebusch
 * Date: 13.04.11
 * Time: 15:42
 *
 * This mechanism will be replaced using SpecificDescriptions and specialized Entities.
 */

object NamingAspect{
  private val symbol = simx.core.ontology.types.OntologySymbol('naming)
  def apply() : GroundedSymbol = symbol
}

case class NameIt(name : String) extends EntityAspect(NamingAspect(), Symbols.name, Nil){
  NamingComponent.self
  def getFeatures = Set(Name).map( _.addAnnotations(Symbols.identifier) )

  def getCreateParams =
    addCVar(Name.addAnnotations(Symbols.identifier)(name))

  def getProvidings =
    getFeatures
}

object NamingComponent extends SingletonComponent(new NamingComponent, NamingAspect(), NamingAspect().value.toSymbol )
{
  def giveName(name : String) : EntityAspect =
    NameIt(name)
}

protected class NamingComponent protected[naming]() extends Component with EntityConfigLayer{


  /**
   * the components name (used for lookup functionality)
   * @return the components name
   */
  def componentName: Symbol = NamingComponent.componentName

  /**
   * the components type (used for lookup functionality)
   * @return the components type
   */
  def componentType: GroundedSymbol = NamingComponent.componentType

  protected def removeFromLocalRep(e: Entity) {}
  override protected def entityConfigComplete(e: Entity, aspect: EntityAspect) {
  }

  protected def configure(params: SValSet) {}

  override protected def requestInitialValues(toProvide: Set[ConvertibleTrait[_]],
                                          aspect: EntityAspect, e: Entity, given: SValSet) {
    provideInitialValues(e, aspect.getCreateParams.combineWithValues(toProvide)._1)
  }

  protected def performSimulationStep() {
    this.simulationCompleted()
  }
}