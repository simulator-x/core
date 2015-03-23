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

package simx.core.ontology.entities

import simx.core.ontology.types.EntityDescription
import simx.core.ontology.Symbols.{entityCreation, entityDescription}
import simx.core.entity.description.{NamedSValSet, EntityAspect}
import simx.core.entity.Entity
import simx.core.ontology.SpecificDescription
import simx.core.entity.component.EntityCreationHandling
import simx.core.svaractor.semantictrait.base.GroundedSymbolFeatures

/**
 * @author dwiebusch
 * Date: 06.11.11
 * Time: 19:42
 */

protected[core] class Subelement( val desc : SpecificDescription )
  extends EntityAspect(entityCreation, entityDescription, Nil)
{
  def getCreateParams = new NamedSValSet(aspectType, EntityDescription(desc))
  def getProvidings   = Set(desc.typeDef.asConvertibleTrait)
  def getFeatures     = Set(desc.typeDef.asConvertibleTrait)

  override def toString: String =
    "Subelement aspect containing " + desc

  def realize(h : Entity => Any = _ => {})( implicit entityCreationContext : EntityCreationHandling ) =
    desc.realize(h)
}