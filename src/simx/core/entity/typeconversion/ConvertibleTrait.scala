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

import simx.core.svaractor.unifiedaccess.Relation

import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag
import simx.core.svaractor.StateParticle
import simx.core.entity.description.SVal
import simx.core.ontology._
import simx.core.ontology.types.{OntologySymbol, NullType}
import scala.language.existentials

/* author: dwiebusch
 * date: 27.08.2010
 */


object TypeInfo{
  final type DataTag[dataType] = TypeTag[dataType]

  def isSubtypeOf(typeInQuestion : DataTag[_], superType : DataTag[_]) : Boolean =
    typeInQuestion.tpe <:< superType.tpe
}

trait TypeInfo[+T, dataType <: T] extends Serializable{
  type baseType
  //!
  val classTag           : ClassTag[dataType]
  //! the semantics of the represented type
  val semantics          : GroundedSymbol
  //! the identifier which is used to inject and lookup svars into/from entities
  def sVarIdentifier     : Symbol = semantics.value.toSymbol
  //! the class manifest of the represented type (should become a typetag some day)
  def typeTag            : TypeInfo.DataTag[dataType]
  //!
  def annotations        : Set[Annotation]
  //!
  def asConvertibleTrait : ConvertibleTrait[dataType]
  //! the base of this convertible trait
  def getBase            : ConvertibleTrait[baseType]
  //!
  val ontoLink           : Option[String]
  //!
  val isCoreType =
    getClass.getPackage == classOf[NullType].getPackage || getClass.getPackage == SVarDescription.getClass.getPackage ||
      getClass.getPackage == classOf[Relation].getPackage
  //!
  final def isSubtypeOf[U](that : TypeInfo.DataTag[U]) : Boolean =
  TypeInfo.isSubtypeOf(typeTag, that)
  //!
  final def isSubtypeOf[U](that : TypeInfo[U, _]) : Boolean =
    TypeInfo.isSubtypeOf(typeTag, that.typeTag)

  /**
   * Checks if this [[TypeInfo]] is of the same semantics as moreSpecialTypeInfo and
   * if all annotations of this [[TypeInfo]] are also present in moreSpecialTypeInfo.
   */
  def >@(moreSpecialTypeInfo: TypeInfo[_,_]): Boolean =
    semantics == moreSpecialTypeInfo.semantics &&
      (annotations.isEmpty || annotations.forall(moreSpecialTypeInfo.annotations.contains))
}

/**
 *  the convertible trait, which provides the information needed to create Provides and Requires
 */
trait ConvertibleTrait[T1] extends Serializable with TypeInfo[T1, T1] {
  //!
  def asConvertibleTrait = this
  //! The represented type.
  type dataType      = T1
  //! returns a copy of this convertible trait containing the old and additionally given annotations
  def addAnnotations( additionalAnnotations : Annotation* ) : ConvertibleTrait[T1] =
    setAnnotations(annotations.union(additionalAnnotations.toSet).toSeq :_*)
  //! returns a copy of this convertible trait containing the annotations
  def setAnnotations( additionalAnnotations : Annotation* ) : ConvertibleTrait[T1]
  //! if this evaluates to true, provided as will create an sval instead of an svar
  def isSValDescription : Boolean
  //! returns a copy that will yield svals instead of svars
  def asConst : ConvertibleTrait[T1]

  def createConverter[T2](in : StateParticle[T1], out : ConvertibleTrait[T2], reverter : Option[IReverter[T2, _]] = None) =
    if (out.typeTag == typeTag)
      in.asInstanceOf[StateParticle[T2]]
    else reverter match {
      case None => in as requiredAs(out).wrapped
      case Some(r : IReverter[_, _]) if r.canRevert(out, this) =>
        in as requiredAs(out).using(r.asInstanceOf[IReverter[T2, T1]]).wrapped
      case Some(r) => throw new InvalidConverterException
    }

  def convert[T2]( to : ConvertibleTrait[T2] )( value : T1 ) : T2 =
    if (this equals to) value.asInstanceOf[T2] else requiredAs(to).wrapped.accessReverter().revert(value)

  /**
   * returns a Provide that creates a non-conversion-ConversionInfo
   * @return a Provide that creates a non-conversion-ConversionInfo
   */
  def isProvided : Provide[T1, _] = {
    implicit val ct  = classTag
    //implicit val tt  = typeTag
    providedAs(this)
  }

  /**
   * returns a Require that creates a non-conversion-ConversionInfo
   * @return a Require that creates a non-conversion-ConversionInfo
   */
  def isRequired : Require[_, T1] =
    requiredAs(this)

  /**
   * returns an Own containing this ConvertibleTrait
   * @return an Own containing this ConvertibleTrait
   */
  def isOwned : Own[T1] =
    Own(this)(classTag)

  /**
   * creates a Provide that will create a svar of type T1 and inject a svar of type T2
   * @param o information on the type of the svar to be injected
   * @return a Provide that will create a svar of type T1 and inject a svar of type T2
   */
  final def providedAs[T2: ClassTag](o: ConvertibleTrait[T2]) =
    if (isSValDescription)
      Provide(new ProvideConversionInfo[T1, T2](this, o, annotations).asConst)
    else
      Provide(new ProvideConversionInfo[T1, T2](this, o, annotations))

  /**
   * creates a Require that will look up a svar of type T1 and return it as a svar of type T2
   * @param o information on the type of the svar to be returned when the lookup has finished
   * @return a Require that will look up a svar of type T1 and return it as a svar of type T2
   */
  final def requiredAs[T2](o: ConvertibleTrait[T2]) =
    Require(new RequireConversionInfo[T1, T2](this, o))

  /**
   *
   * Augments the ConvertibleTrait with a value to create a complete CreateParam.
   */
  def apply(value: T1) : SVal[T1,TypeInfo[T1,T1]] =
    SVal.apply[T1,T1](this)(value)(classTag)

  //TODO: Keep in mind
  def apply[TType >: this.type <: TypeInfo[T1,T1]](typeInfo: TType, value: T1) : SVal[T1,TType] =
    SVal.applyAs[T1,T1,TType](typeInfo)(value)(classTag)

  override def toString =
    sVarIdentifier.name + " (" + typeTag.toString() +
      (if (annotations.isEmpty) ")" else " (with annotations " + annotations.map(
        x => x.value + (if (x.typedSemantics == OntologySymbol) "" else " (" + x.typedSemantics.sVarIdentifier.name +")")
      ).mkString(" and ") + ")")

  override def equals(p1: Any) = p1 match {
    case p : ConvertibleTrait[_] =>
      p.sVarIdentifier == sVarIdentifier &&
        p.annotations.diff(annotations).isEmpty &&
        annotations.diff(p.annotations).isEmpty &&
        p.typeTag == typeTag
    case _ => false
  }

  override def hashCode() =
    41 * (41 + (41 * (41 + sVarIdentifier.hashCode()) + annotations.hashCode())) + typeTag.hashCode()
}

case class NoBaseDefinedException( from : ConvertibleTrait[_], to : ConvertibleTrait[_] )
  extends Exception("Cannot convert to " + to + " because base is defined for ConvertibleTrait " + from)

class InvalidConverterException extends Exception