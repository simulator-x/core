/*
 * Copyright 2014 The SIRIS Project
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

package simx.core.svaractor.semantictrait

import java.util.UUID

import simx.core.entity.description.SVal.SValType
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.ontology.Symbols
import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.semantictrait.base._
import simx.core.svaractor.semantictrait.base.Semantic.Value
import simx.core.svaractor.semantictrait.example.symbols
import simx.core.svaractor.semantictrait.example.types.SemanticEntity
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling


/**
 * Created by dwiebusch on 27.11.14
 */
class SemanticEntityBase[+T <: Thing](gs : GroundedSymbolBase[T], val entity : SemanticEntity.ValueType)
  extends SemanticValue[SemanticEntity.DataType, Symbols.entity.SymbolType with T]
{
  override val valueDescription = SemanticEntity.valueDescription.extendBy(gs)
  override val value: SemanticEntity.DataType = entity.value

  override def toString: String = "SemanticEntity#" + entity.value.id

  override def attain(v: Value[_]*)(implicit context: EntityUpdateHandling): Option[SemanticValue[Any, Thing]]@CPSRet = entity.attain(v :_*)

  override def attainR[X, B <: Base, S <: Thing](retTyp: SemanticTypeTrait[X, B, S], v: Value[_]*)(implicit context: EntityUpdateHandling): Option[SemanticValue[X, S]]@CPSRet =
    entity.attainR(retTyp, v :_*)
}


protected trait Castability extends SemanticValue[SemanticEntity.DataType, Symbols.entity.SymbolType]{
  def as[V <: Thing](that : SpecificSemanticTrait[V])(implicit context : EntityUpdateHandling) =
    that.tryApply(this)
}

