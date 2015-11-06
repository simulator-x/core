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

import simx.core.entity.description._
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.Entity
import simx.core.ontology.entities.Subelement
import simx.core.svaractor.SVarActor
import simx.core.svaractor.semantictrait.base.BaseValueDescription

/**
 * @author dwiebusch
 * Date: 06.09.11
 * Time: 16:22
 */

class EntityDescription(aspects : List[EntityAspect],
                        name : Symbol = Symbol("unnamed-entity"),
                        path : List[Symbol] = Nil,
                        annotations: Set[Annotation] = Set(),
                        additionalProperties: SValSet = SValSet())
  extends SpecificEntityDescription(
    if(annotations.isEmpty) PlainEntity else PlainEntity.setAnnotations(annotations), aspects, name, path, Seq(), additionalProperties)
  with Serializable {
  def this(name : String, aspects : EntityAspect*) = this(aspects.toList, Symbol(name))
  def this(aspects : EntityAspect*) = this(aspects.toList)

  def this(_name: Symbol, _additionalProperties: SVal.SValType[_]*) = this(
    aspects = List[EntityAspect](),
    name = _name,
    path = List[Symbol](),
    annotations = Set[Annotation](),
    additionalProperties = SValSet(_additionalProperties:_*)
  )
}

class SpecificEntityDescription(entityDesc : EntitySValDescription,
                                val aspects    : List[EntityAspect],
                                name       : Symbol,
                                path       : List[Symbol] = Nil,
                                features   : Seq[ConvertibleTrait[_]] = Seq(),
                                additionalProperties: SValSet = SValSet())
  extends Subelement(new SpecificDescription(entityDesc, aspects, name, path, features, additionalProperties)) with Serializable
{
  def this(entityDesc : EntitySValDescription, aspects : List[EntityAspect], features : ConvertibleTrait[_]* ) =
    this(entityDesc, aspects, Symbol("unnamed-entity"), Nil, features)
}


protected[core] class SpecificDescription( entityDesc : EntitySValDescription,
                                           aspects    : List[EntityAspect],
                                           name       : Symbol,
                                           path       : List[Symbol] = Nil,
                                           features   : Seq[ConvertibleTrait[_]] = Seq(),
                                           additionalProperties: SValSet = SValSet())

  extends GeneralEntityDescription(entityDesc, entityDesc.ctor, None, path :+ name,
    if (features.nonEmpty) FeatureDefinition(features.toSet) :: aspects else aspects, additionalProperties)
{
  def this(entityDesc : EntitySValDescription, aspects : List[EntityAspect], features : ConvertibleTrait[_]* ) =
    this(entityDesc, aspects, Symbol("unnamed-entity"), Nil, features)
}


object _PlainEntity extends simx.core.svaractor.semantictrait.base.BasicGroundedSymbol

object PlainEntity extends EntitySValDescription(
  BaseValueDescription(_PlainEntity),
  (entity: Entity, svarActor: SVarActor) => new Entity(entity)(svarActor), types.Entity.ontoLink.get) with Serializable

private case class FeatureDefinition(getFeatures : Set[ConvertibleTrait[_]])
  extends EntityAspect(Symbols.nullType, Symbols.nullType, Nil)
{
  def getCreateParams = throw FeatureDefinitionException()
  def getProvidings   = Set()
}

private case class FeatureDefinitionException()
  extends Exception("FeatureDefinition instances are only meant to define features, don't use them for entity creation")
