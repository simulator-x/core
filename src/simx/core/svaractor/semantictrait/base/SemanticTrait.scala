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

package simx.core.svaractor.semantictrait.base

import simx.core.ontology.Symbols
import simx.core.svaractor.SVarActor
import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.semantictrait.base.Semantic.Value
import simx.core.svaractor.semantictrait.base.TraitTypes.PartialEntityRelation
import simx.core.svaractor.semantictrait.example.symbols
import simx.core.svaractor.semantictrait.example.types.SemanticEntity
import simx.core.svaractor.semantictrait.SemanticEntityBase
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling
import scala.language.reflectiveCalls

import scala.reflect.runtime.universe.{TypeTag, typeOf}

/**
 * Created by dwiebusch on 27.11.14
 */
object TraitTypes{
  type PartialEntityRelation = PartialRelation[_ <: SemanticEntity.DataType, _, _ <: Thing]
}

abstract class SemanticTrait[V <: Thing : TypeTag](gs : GroundedSymbolBase[V], desc : PartialEntityRelation*)
  extends SpecificSemanticTrait[V](gs, desc :_*)
{
  final type SpecificEntityType = SemanticEntity

  final protected def createEntity(e: SemanticEntity.ValueType)(implicit creator : SVarActor): SpecificEntityType =
    new SemanticEntity(e)
}

object SemanticTrait extends BasicGroundedSymbol{
  val valueDesc = BaseValueDescription(this)
}

abstract class SpecificSemanticTrait[V <: Thing : TypeTag](val gs : GroundedSymbolBase[V], protected val desc : PartialEntityRelation*)
  extends SemanticValue[SpecificSemanticTrait[V], SemanticTrait.SymbolType]
{
  import CPST.cpsIterable
  protected type SpecificEntityType <: SemanticType
  final type SemanticType = SemanticValue[SemanticEntity.DataType, Symbols.entity.SymbolType with V]
  final type Type = V

  protected class SemanticEntity(entity : SemanticEntity.ValueType)
    extends SemanticEntityBase[V](gs, entity)

  override def subsumes(that: Value[_])(implicit context: EntityUpdateHandling) : Boolean@CPSRet =
    desc.cps.forall(_.subsumes(that)) && classes.cps.forall(that.matches(_)(context))

  override val valueDescription: ValueDescription[Base, SemanticTrait.SymbolType] =
    SemanticTrait.valueDesc

  override val value: SpecificSemanticTrait[V] =
    this


  override def equals(obj: Any): Boolean = obj match {
    case that : SpecificSemanticTrait[_] => that.desc == desc && that.classes == classes
    case _ => false
  }

  override def hashCode(): Int =
    desc.hashCode() + 41 * classes.hashCode()

  override def toString: String =
    "SemanticTrait " + getClass.getSimpleName

  protected def createEntity(e : SemanticEntity.ValueType)(implicit creator : SVarActor) : SpecificEntityType

  val classes =
    SemanticTypeTrait.lookUp(typeOf[V])

  def tryApply(v : SemanticEntity.ValueType)(implicit context: EntityUpdateHandling) : Option[SpecificEntityType]@CPSRet = {
    val checkResult = (desc.map(_.asInstanceOf[Semantic.Value[_]]) ++ classes).cps.foldLeft(List[Semantic.Value[_]]()){
      (result, req) => if (v matches req) result else req :: result
    }
    if (checkResult.isEmpty)
      Some(createEntity(v))
    else
      None //throw new Exception("Not wrappable: " + checkResult.mkString(" and ") + " not found for entity with id " + v.value)
  }

  def apply(e : SemanticValue[SemanticEntity.DataType, Symbols.entity.SymbolType with V])(implicit context: EntityUpdateHandling) : SpecificEntityType@CPSRet =
    tryApply(e).get
}