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

package simx.core.ontology

import simx.core.entity.description.{EntityAspect, GeneralEntityDescription}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.Entity

/**
 * @author dwiebusch
 * Date: 06.09.11
 * Time: 16:22
 */

class EntityDescription( override val aspects : List[EntityAspect])
  extends SpecificDescription[Entity](PlainEntity, aspects.toList) {
  def this(aspects : EntityAspect*) = this(aspects.toList)
}



protected abstract class SpecificDescription[T <: Entity]( entityDesc : EntitySVarDescription[T],
                                                 aspects    : List[EntityAspect],
                                                 features   : ConvertibleTrait[_]* )
  extends GeneralEntityDescription[T](entityDesc, entityDesc.ctor, None,
    if (features.nonEmpty) FeatureDefinition(features.toSet) :: aspects else aspects)


private object PlainEntity extends EntitySVarDescription[Entity](Symbols.entity, new simx.core.entity.Entity(_) {
  override protected def getSimpleName = "PlainEntity"
})

private case class FeatureDefinition(getFeatures : Set[ConvertibleTrait[_]])
  extends EntityAspect(Symbols.nullType, Symbols.nullType, Nil)
{
  def getCreateParams = throw FeatureDefinitionException()
  def getProvidings   = Set()
}

private case class FeatureDefinitionException()
  extends Exception("FeatureDefinition instances are only meant to define features, don't use them for entity creation")
