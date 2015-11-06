/*
 * Copyright 2015 The SIRIS Project
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

package simx.core.worldinterface.entity.filter

import simx.core.entity.Entity
import simx.core.entity.description.SVal
import simx.core.entity.typeconversion.{ConvertibleTrait, TypeInfo}
import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.semantictrait.base.{Base, Thing}

/**
 * Created by martin 
 * on 05/05/15.
 */
class SemanticValueSatisfies[T](semanticType: ConvertibleTrait[T], accept: T => Boolean, matchAnnotationsBidirectionally: Boolean = true) extends EntityFilter {

  lazy val _apply: (Entity) => Boolean@CPSRet =
    if(matchAnnotationsBidirectionally) matchBidirectionally else matchUnidirectionally

  private def matchUnidirectionally(e: Entity) = {
    val valueInEntity = Result of e.get(semanticType).headOption
    valueInEntity.exists(accept)
  }

  private def matchBidirectionally(e: Entity) = {
    val sVar = e.getAllStateParticles.find(sp => {
      (sp.typeInfo.getBase == semanticType.getBase) && (sp.identifier == semanticType.sVarIdentifier) && (sp.annotations == semanticType.annotations)
    })
    if(sVar.isDefined)
      accept(sVar.get.svar.read.asInstanceOf[T])
    else
      false
  }

  override def apply(e: Entity) = _apply(e)
}

object HasSemanticValue {
  @deprecated("Use simx.core.worldinterface.entity.filter.HasSVal instead", "07-08-2015")
  def apply[T](semanticType: ConvertibleTrait[T]) = HasSVal(semanticType)
}

case class HasSVal[T](semanticType: ConvertibleTrait[T]) extends EntityFilter {
  override def apply(e: Entity) = {
    val valueInEntity = Result of e.get(semanticType).headOption
    valueInEntity.isDefined
  }
  override def toString(): String = "HasSVal[" + semanticType.toString + "]"
}

case class SValEquals[T, TTypeInfo<:TypeInfo[T,T], B<:Base, S<:Thing](semanticValue: SVal[T,TTypeInfo,B,S], matchAnnotationsBidirectionally: Boolean = true)
  extends SemanticValueSatisfies(
    semanticType = semanticValue.asConvertibleTrait,
    accept = (value: T) => value == semanticValue.value,
    matchAnnotationsBidirectionally
  ) {
  override def toString(): String = "SValEquals[" + semanticValue.toString + "]"
}

object SemanticValueEquals {
  @deprecated("Use simx.core.worldinterface.entity.filter.SValEquals instead", "22-06-2015")
  def apply[T, TTypeInfo<:TypeInfo[T,T], B<:Base, S<:Thing](semanticValue: SVal[T,TTypeInfo,B,S], matchAnnotationsBidirectionally: Boolean = true) =
    SValEquals(semanticValue, matchAnnotationsBidirectionally)
}

case class NameContains(nameFragment: String) extends EntityFilter {
  override def apply(e: Entity) = {
    e.getSimpleName.toLowerCase.contains(nameFragment.toLowerCase)
  }
  override def toString(): String = "NameContains[" + nameFragment.toLowerCase + "]"
}