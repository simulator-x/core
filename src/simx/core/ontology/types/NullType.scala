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
import simx.core.entity.description.{Semantics => CoreSemantics, SVal}
import scala.reflect.ClassTag

/**
 * User: dwiebusch
 * Date: 11.04.11
 * Time: 17:09
 */
object OntologySymbol extends SVarDescription[CoreSemantics, CoreSemantics](NullType(OSymBase)) with Serializable{
  def apply(s : Symbol) : GroundedSymbol = CoreSemantics(s) asGroundedSymbol this
}

class NullType private(name : GroundedSymbol) extends Serializable {
  def withType[U : DataTag : ClassTag]( c : Class[U] ) : SVarDescription[U, U] = NullType apply new CBase[U]{
    val ontoLink          = Some("http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#NullType")
    def isSValDescription = false
    val semantics         = name
  }
}

private abstract class CBase[U : ClassTag : DataTag] extends ConvertibleTrait[U] with Serializable{
  //! returns a copy of this convertible trait containing the annotations
  override def setAnnotations(additionalAnnotations: Annotation*) = this
  def asConst     = throw new Exception("must not call asConst on CBase")
  def typeTag     = implicitly[TypeInfo.DataTag[U]]
  val classTag    = implicitly[ClassTag[U]]
  def annotations = Set()
  def getBase     = this
  type baseType   = U
}

private object OntologySymbolBase extends CoreSemantics('OntologySymbol) with Serializable

private object OSymBase extends CBase[CoreSemantics] with Serializable{
  val semantics         = SVal(this)(OntologySymbolBase)
  val ontoLink          = None
  def isSValDescription = true
}

//Base class for generated OntologyMembers
object NullType {
  def as( semantics : GroundedSymbol ) =
    new NullType(semantics)

  private[types] def apply[T : ClassTag : DataTag]( o : ConvertibleTrait[T] ) : SVarDescription[T, T] =
    new SVarDescription[T, T](o.semantics, o, o.isSValDescription, o.annotations, o.ontoLink){
      override def definedAt(iri: String) =
        new SVarDescription(semantics, getBase, isSValDescription, annotations, Some(iri)){
          override val getBase = this
        }
    }
}
