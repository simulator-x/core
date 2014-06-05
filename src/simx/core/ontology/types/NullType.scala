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

import reflect.runtime.universe.TypeTag
import simx.core.ontology.SVarDescription
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.description.{SVal, Semantics => CoreSemantics}
import simx.core.ontology.GroundedSymbol
import scala.reflect.ClassTag

/**
 * User: dwiebusch
 * Date: 11.04.11
 * Time: 17:09
 */

//Base class for generated OntologyMembers
object NullType{
  def as( semantics : GroundedSymbol ) = new NullType(semantics.value)
  def as( semantics : CoreSemantics ) = new NullType(semantics)
}

class NullType private(name : CoreSemantics) extends Serializable {
  def createdBy[U]( ctor : => U )( implicit info : ClassTag[U] ) =
    SVarDescription[U](new ConvertibleTrait[U]{
      implicit val classTag : ClassTag[U] = info
      type baseType      = U
      val annotations    = Set[GroundedSymbol]()
      def addAnnotations(additionalAnnotations: GroundedSymbol*) = this
      val ontoLink       = Some("http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#NullType")
      def defaultValue() = ctor
      def semantics      = name
      def typeinfo       = info
      def getBase        = this
    })
}

protected object OntologySymbolBase extends CoreSemantics{ def toSymbol = 'OntologySymbol }
object OntologySymbol extends SVarDescription[CoreSemantics, CoreSemantics](NullType as OntologySymbolBase createdBy OntologySymbolBase){
  def apply(s : Symbol) : GroundedSymbol = new SVal(this)(new CoreSemantics{ def toSymbol = s })
}
