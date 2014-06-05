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

package simx.core.entity.typeconversion

import scala.reflect.runtime.universe.TypeTag
import simx.core.entity.description.{Semantics, SVal}
import java.lang.Exception
import simx.core.svaractor.SVar
import simx.core.entity.InvalidConverterException
import simx.core.ontology.GroundedSymbol
import scala.Some
import scala.reflect.ClassTag

/* author: dwiebusch
 * date: 27.08.2010
 */


trait TypeInfo[+T]{
  type dataType
  type baseType
  //!
  val classTag       : ClassTag[dataType]
  //! the semantics of the represented type
  def semantics      : Semantics
  //! the identifier which is used to inject and lookup svars into/from entities
  val sVarIdentifier : Symbol = semantics.toSymbol
  //! the class manifest of the represented type
  def typeinfo       : ClassTag[dataType]
  //!
  def annotations    : Set[GroundedSymbol]
  //!
  def asConvertibleTrait : ConvertibleTrait[dataType]
  //! the base of this convertible trait
  def getBase        : ConvertibleTrait[baseType]
  //!
  val ontoLink       : Option[String]
}

/**
 *  the convertible trait, which provides the information needed to create Provides and Requires
 */
trait ConvertibleTrait[T1] extends Serializable with TypeInfo[T1] {
  //!
  def asConvertibleTrait = this
  //! The represented type.
  type dataType      = T1
  //! the class manifest of the represented type
  def typeinfo       : ClassTag[T1]
  //! returns a copy of this convertible trait containing the annotations
  def addAnnotations( additionalAnnotations : GroundedSymbol* ) : ConvertibleTrait[T1]

  def createConverter[T2]( in : SVar[T1], out : ConvertibleTrait[T2], reverter : Option[IReverter[T2, _]] = None ) =
    if (out.typeinfo == typeinfo)
      in.asInstanceOf[SVar[T2]]
    else reverter match {
      case None => new ConvertedSVar(in, this requiredAs out)
      case Some(r : IReverter[_, _]) if r.canRevert(out, this) =>
        new ConvertedSVar(in, this requiredAs out using r.asInstanceOf[IReverter[T2, T1]])
      case Some(r) => throw new InvalidConverterException
    }

  def convert[T2]( to : ConvertibleTrait[T2] )( value : T1 ) : T2 =
    requiredAs(to).wrapped.accessReverter().revert( value )

  /**
   * creates an instance of the represented type
   * @return an instance of the represented type
   */
  def defaultValue() : T1

  /**
   * returns a Provide that creates a non-conversion-ConversionInfo
   * @return a Provide that creates a non-conversion-ConversionInfo
   */
  def isProvided : Provide[T1, _] = providedAs(this)(typeinfo)

  /**
   * returns a Require that creates a non-conversion-ConversionInfo
   * @return a Require that creates a non-conversion-ConversionInfo
   */
  def isRequired : Require[_, T1] = requiredAs(this)

  /**
   * returns an Own containing this ConvertibleTrait
   * @return an Own containing this ConvertibleTrait
   */
  def isOwned : Own[T1] = Own(this)

  /**
   * creates a Provide that will create a svar of type T1 and inject a svar of type T2
   * @param o information on the type of the svar to be injected
   * @return a Provide that will create a svar of type T1 and inject a svar of type T2
   */
  final def providedAs[T2](o: ConvertibleTrait[T2])(implicit tt : ClassTag[T2]) =
    Provide(new ProvideConversionInfo[T1, T2](this, o, annotations))

  /**
   * creates a Require that will look up a svar of type T1 and return it as a svar of type T2
   * @param o information on the type of the svar to be returned when the lookup has finished
   * @return a Require that will look up a svar of type T1 and return it as a svar of type T2
   */
  final def requiredAs[T2](o: ConvertibleTrait[T2]) = Require(new RequireConversionInfo[T1, T2](this, o))

  /**
   *
   * Augments the ConvertibleTrait with a value to create a complete CreateParam.
   */
  def apply(value: T1) : SVal[T1] =
    SVal(this)(value)

  override def toString =
    sVarIdentifier.name + (if (annotations.isEmpty) "" else " (with annotations + " + annotations.mkString(", ") + ")")

  override def equals(p1: Any) = p1 match {
    case p : ConvertibleTrait[_] =>
      p.sVarIdentifier == sVarIdentifier &&
        p.annotations.diff(annotations).isEmpty &&
        annotations.diff(p.annotations).isEmpty &&
        p.typeinfo == typeinfo
    case _ => false
  }

  override def hashCode() =
    41 * (41 + (41 * (41 + sVarIdentifier.hashCode()) + annotations.hashCode())) + typeinfo.hashCode()
}

case class NoBaseDefinedException( from : ConvertibleTrait[_], to : ConvertibleTrait[_] )
  extends Exception("Cannot convert to " + to + " because base is defined for ConvertibleTrait " + from)