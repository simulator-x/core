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
import simx.core.ontology._
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.description.{Semantics => CoreSemantics, SVal}
import scala.reflect.ClassTag
import scala.Some

/**
 * User: dwiebusch
 * Date: 11.04.11
 * Time: 17:09
 */

protected abstract class CBase[U : ClassTag : TypeTag] extends ConvertibleTrait[U]{
  //! returns a copy of this convertible trait containing the annotations
  override def setAnnotations(additionalAnnotations: Annotation*) = this

  val classTag    = implicitly[ClassTag[U]]
  def typeTag     = implicitly[TypeTag[U]]
  def annotations = Set()
  def getBase     = this
  def asConst     = ???
  type baseType   = U
}

//Base class for generated OntologyMembers
object NullType {
  def as( semantics : GroundedSymbol ) =
    new NullType(semantics)
}

class NullType private(name : GroundedSymbol) extends Serializable {
  @deprecated("use withType instead", "")
  def createdBy[U : TypeTag : ClassTag]( ctor : => U ) =
    withType[U](ctor.getClass.asInstanceOf[Class[U]])

  def withType[U : TypeTag : ClassTag]( c : Class[U] ) : SVarDescription[U, U] = SVarDescription apply new CBase[U]{
    val ontoLink          = Some("http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#NullType")
    def isSValDescription = false
    val semantics         = name
  }
}

private object OSymBase extends CBase[CoreSemantics]{
  protected object OntologySymbolBase extends CoreSemantics with Serializable{ def toSymbol = 'OntologySymbol }
  val semantics         = SVal(this)(OntologySymbolBase)
  val ontoLink          = None
  def isSValDescription = true
}

object OntologySymbol extends SVarDescription[CoreSemantics, CoreSemantics](SVarDescription(OSymBase)) with Serializable{
  def apply(s : Symbol) : GroundedSymbol = new SVal(this)(new CoreSemantics{ def toSymbol = s })
}
