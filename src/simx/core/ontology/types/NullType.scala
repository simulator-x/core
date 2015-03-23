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

package simx.core.ontology.types

import simx.core.entity.typeconversion.TypeInfo._

import simx.core.ontology._
import simx.core.entity.typeconversion.{TypeInfo, ConvertibleTrait}
import simx.core.entity.description.SVal
import simx.core.svaractor.semantictrait.base._
import scala.reflect.{classTag, ClassTag}

/**
 * User: dwiebusch
 * Date: 11.04.11
 * Time: 17:09
 */

abstract class OntologySymbol(s : Symbol) extends simx.core.svaractor.semantictrait.base.BasicGroundedSymbol with Serializable{
  def asOntoSym = OntologySymbol(this)
}

object OntologySymbol extends SValDescription[GroundedSymbolBase[Thing], GroundedSymbolBase[Thing], Base, OntologySymbolBase.SymbolType](NullType(OSymBase)) with Serializable{
  def apply(s : GroundedSymbol) : SVal.SValType[GroundedSymbolBase[_ <: Thing]] =
    SVal.apply(this, BaseValueDescription(OntologySymbolBase))(s.Symbol)
}

class NullType[X <: Base, S <: Thing] private(name : GroundedSymbolBase[S]) extends Serializable {
  def withType[U : DataTag : ClassTag]( c : Class[U] ) : SValDescription[U, U, Base, S] = NullType apply new CBase[U, S](name){
    val ontoLink          = Some("http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#NullType")
    def isSValDescription = false
    val semantics         = valueDescription.groundedSymbol
  }
}

private abstract class CBase[U : ClassTag : DataTag, S <: Thing](name : GroundedSymbolBase[S]) extends ConvertibleTrait[U] with Serializable{
  val valueDescription : ValueDescription[Base, S]  = BaseValueDescription(name)
  override def setAnnotations(additionalAnnotations: Set[Annotation]) = this
  def asConst     = throw new Exception("must not call asConst on CBase")
  def typeTag     = implicitly[TypeInfo.DataTag[U]]
  val classTag    = implicitly[ClassTag[U]]
  def annotations = Set()
  def getBase     = this
  type baseType   = U

  override def apply(value: U): SVal[U, TypeInfo[U, U], _ <: Base, _ <: Thing] =
    SVal(this, valueDescription)(value)(classTag)
}

object OntologySymbolBase extends BasicGroundedSymbol with Serializable

private object OSymBase extends CBase[GroundedSymbolBase[Thing], OntologySymbolBase.SymbolType](OntologySymbolBase.Symbol) with Serializable{
  val semantics         = OntologySymbolBase.Symbol
  val ontoLink          = None
  def isSValDescription = true
}

//Base class for generated OntologyMembers
object NullType extends Serializable{
  def as[S <: GroundedSymbolFeatures](semantics : S) =
    new NullType[Base, S#SymbolType](semantics.Symbol)

  private[types] def apply[T : ClassTag : DataTag, S <: Thing]( o : CBase[T, S] ) : SValDescription[T, T, Base, S] =
    new SValDescription[T, T, Base, S](classTag[T].runtimeClass.asInstanceOf[Class[T]], o.valueDescription, o, o.isSValDescription, o.annotations, o.ontoLink){
      override def definedAt(iri: java.lang.String) =
        new SValDescription(classType, valueDescription, getBase, isSValDescription, annotations, Some(iri)){
          override val getBase = this
        }

      override def toString: java.lang.String = "incomplete description for " + o.typeTag.tpe
    }
}
