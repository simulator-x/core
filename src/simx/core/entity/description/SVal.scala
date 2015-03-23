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

package simx.core.entity.description

import simx.core.entity.description.SVal.SValType
import simx.core.svaractor.TimedRingBuffer.{Now, ContentType, Time}
import simx.core.svaractor.semantictrait.base._
import simx.core.svaractor.{AccessMethod, StateParticle, SVarActor, ImmutableSVar}
import simx.core.entity.typeconversion._
import simx.core.ontology._
import scala.reflect.ClassTag


object SVal{
  def apply[T : ClassTag, V <: T, X <: Base, S <: Thing]( semantics : TypeInfo[T,T], valueDescription: ValueDescription[X, S])( value : V ) : SVal[T,TypeInfo[T,T], X, S] =
    new SVal[T,TypeInfo[T,T], X, S] (value, valueDescription, semantics)

//  def applyAs[T : ClassTag, V <: T, TType <: TypeInfo[T,T], X <: Base, S <: Thing]( semantics : TType )( value : V ) : SVal[T,TType, X, S] =
//    new SVal[T,TType, X, S] (semantics)(value)

  type SValType[T] = SVal[T,TypeInfo[T,T], _ <: Base, _ <: Thing ]
}

class SVal[T, +TType <: TypeInfo[T,T], +X <: Base, +S <: Thing](value : T, val valueDescription: ValueDescription[X, S], typedSemantics : TypeInfo[T, T])
  extends SValBase[T, T](typedSemantics, value) with SemanticValue[T, S] with StateParticle[T] with Serializable
{
  def asSVal : SVal[T,TypeInfo[T,T], X, S] =
    this

  def typedValue: T =
    value

  def asConvertibleTrait: ConvertibleTrait[T] =
    typedSemantics.asConvertibleTrait

  def as(gs : GroundedSymbol) =
    SVal(typedSemantics.asConvertibleTrait.addAnnotations(gs), valueDescription)(value)(typedSemantics.classTag)

  //def this(_value : T, _valueDescription: ValueDescription[X, S], _typedSemantics : TypeInfo[T, T]) = this(_typedSemantics)(_value, _valueDescription)
}

case class WrappedSVal[T,TType <: ConvertibleTrait[T],R,RType](sVal: (T,TType), func: ((T,TType))  => (R,RType)) {
  def calculate =
    func.apply(sVal)._1
}

case class Executor[T,TType<:TypeInfo[T,T],R,RType <: TypeInfo[R,R]](sVal: SVal[T,TType, _ <: Base, _<: Thing], func: SVal[T,TType, _ <: Base, _<: Thing] => SVal[R,RType, _ <: Base, _<: Thing]) {
  def execute() = func(sVal)
}

/**
 *
 * This class represents a value with a semantics.
 */
sealed abstract class SValBase[+T, B <: T] protected (val typedSemantics: TypeInfo[T, B], val value: T)
  extends ImmutableSVar[T, B] with Serializable
{
  def asSVal : SVal[B,TypeInfo[B,B], _ <: Base, _ <: Thing]

  def typedValue: B

  override def getValue: Option[B] =
    Some(typedValue)

  def observe(handler: (T, Time) => Unit, ignoredWriters: Set[SVarActor.Ref] = Set())(implicit actorContext: SVarActor) = {
    handler(value, Now)
    java.util.UUID.randomUUID()
  }

  override def get(time: Time, accessMethod: AccessMethod)(handler: ContentType[T] => Unit)(implicit actorContext: SVarActor): Unit ={
    handler(value -> time)
  }

  private val baseValue =
    Converter(typedSemantics.asConvertibleTrait, typedSemantics.getBase).convert(typedValue)

  def asProvide =
    typedSemantics.asConvertibleTrait.isProvided.withInitialValue(typedValue)

  def asBaseType : SVal.SValType[typedSemantics.baseType] =
    typedSemantics.getBase.apply(baseValue)

  def as[T2](cInfo: ConversionInfo[T2, B]): SVal[T2,TypeInfo[T2,T2], _ <: Base, _<: Thing] =
    cInfo from as(cInfo.from)

  /**
   *
   * Returns the value of this CreateParam in the given type.
   *
   * This method searches and calls a IReverter if needed.
   * If no suitable IReverter is found a NoReverterFoundException is thrown.
   */
  def as[O](outputHint: ConvertibleTrait[O]): O =
    Reverter(outputHint, typedSemantics.getBase ).revert(baseValue)

  def to[O](output : ConvertibleTrait[O]) : SValType[O] =
    output.apply(as(output))



//  def as[T2](p: ProvideAndRequire[T2, T]): SVar[T2] = {
//    implicit val ct = p.convInfo.from.classTag
//    SVal(p.convInfo.from)(as(p.convInfo.from))
//  }

  //! the typeinfo of the type of the contained value
  val containedValueManifest  =
    typedSemantics.classTag

  /**
   *
   * Overwritten equals method to use OntologyMembers in collections.
   *
   * This method was implemented using the guidline from "Programming in Scala" chapter 28.
   *
   * @see Programming in Scala - Odersky, Spoon, Venners - First Edition, Version 6
   */
  override def equals(other: Any): Boolean = other match {
    case that: SVal[_,_,_,_] => typedSemantics.equals(that.typedSemantics) && value.equals(that.value)
    case _             => false
  }

  /**
   *
   * Overwritten hashCode method to use OntologyMembers in collections.
   *
   * This method was implemented using the guidline from "Programming in Scala" chapter 28.
   *
   * @see Programming in Scala - Odersky, Spoon, Venners - First Edition, Version 6
   */
  override def hashCode: Int =
    41 * ( 41 + typedSemantics.hashCode() )  + value.hashCode()
  //41 * ( 41 + typedSemantics.typeinfo.hashCode ) + typedSemantics.sVarIdentifier.hashCode

  override def toString: String = "SVal(" + typedSemantics.classTag.toString + " as " +
    typedSemantics.semantics + ") = " +  value.toString

  def ::[thatT,thatTType<:TypeInfo[thatT,thatT]](that : SVal[thatT,thatTType, _ <: Base, _<: Thing]) : List[SVal[_,_, _ <: Base, _<: Thing]] =
    List[SVal[_,_, _ <: Base, _<: Thing]](asSVal, that)

  def and(that : SVal[_,_ <: TypeInfo[_, _], _ <: Base, _<: Thing]) : SValSeq =
    new SValSeq and asSVal and that

  def and : Seq[SVal[_,_ <: TypeInfo[_, _], _ <: Base, _<: Thing]] => SValSeq =
    _.foldLeft(new SValSeq)( _ and _ )
}

class SValSeq ( cvar : SVal[_,_, _ <: Base, _<: Thing]* ) extends Seq[SVal[_,_, _ <: Base, _<: Thing]]{
  private var internalList = List[SVal[_,_, _ <: Base, _<: Thing]](cvar : _*)
  def iterator = internalList.iterator
  //def apply(idx: Int) = internalList(idx)

  override def apply(idx: Int): SVal[_, _, _ <: Base, _<: Thing] = internalList(idx)

  def length = internalList.length
  def and( cvar : SVal[_,_ <: TypeInfo[_, _], _ <: Base, _<: Thing] ) : SValSeq = { internalList = cvar :: internalList; this }
}


/**
 *
 *
 * This exception is thrown if a NamedSValList can not find a requested CreateParam.
 *
 * @param sVarIdentifier
 * The sVarIdentifier of the CreateParam that was not found.
 */
case class SValNotFound(sVarIdentifier: Symbol) extends Exception(sVarIdentifier.name)

/* author: igg.dwiebusch, igg.mfischbach :P
 * date: 27.08.2010
 */

//case class Semantics(toSymbol: Symbol) extends svaractor.semantictrait.base.GroundedSymbol {
//  assert(toSymbol.name.nonEmpty)
//  def asGroundedSymbol(typeInfo : TypeInfo[Semantics, Semantics]) : GroundedSymbol =
//    SVal.apply[Semantics,Semantics](typeInfo)(this)
//
//  override def toString: String = toSymbol.name
//  override def hashCode() = toSymbol.hashCode()
//  override def equals(p1: Any) = p1 match {
//    case sem : Semantics => sem.toSymbol.equals(toSymbol)
//    case _ => false
//  }
//}

//abstract class SValSemantics(override val value: Semantics) extends SVal[Semantics](OntologySymbol)
