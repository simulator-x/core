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

import simx.core.entity.typeconversion._
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag
import simx.core.svaractor.{StateParticle, SVarActor, ImmutableSVar}


object SVal{
  def apply[T : ClassTag, V <: T]( semantics : TypeInfo[T, T] )( value : V ) : SVal[T] =
    new SVal(semantics)(value)
}

final class SVal[T](typedSemantics : TypeInfo[T, T])(value : T)
  extends SValBase[T, T](typedSemantics, value) with StateParticle[T] with Serializable
{
  protected[description] def asSVal =
    this

  protected def typedValue: T =
    value
}

/**
 *
 * This class represents a value with a semantics.
 */
sealed abstract class SValBase[+T, B <: T] protected (val typedSemantics: TypeInfo[T, B], val value: T)
  extends ImmutableSVar[T, B] with Serializable
{
  protected[description] def asSVal : SVal[B]

  protected def typedValue: B

  private val baseValue =
    Converter(typedSemantics.asConvertibleTrait, typedSemantics.getBase).convert(typedValue)

  def asProvide =
    typedSemantics.asConvertibleTrait.isProvided.withInitialValue(typedValue)


  def get(handler: (T) => Unit)(implicit actorContext: SVarActor){
    handler(value)
  }


  def as[T2](cInfo: ConversionInfo[T2, B]): SVal[T2] =
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
    case that: SVal[_] => typedSemantics.equals(that.typedSemantics) && value.equals(that.value)
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

  override def toString: String = "SVal(" + typedSemantics.typeTag.toString + " as " +
    typedSemantics.semantics.value + ") = " +  value.toString

  def ::(that : SVal[_]) : List[SVal[_]] =
    asSVal :: that :: Nil

  def and(that : SVal[_]) : SValSeq =
    new SValSeq and asSVal and that

  def and : Seq[SVal[_]] => SValSeq =
    _.foldLeft(new SValSeq)( _ and _ )
}

class SValSeq ( cvar : SVal[_]* ) extends Seq[SVal[_]]{
  private var internalList = List[SVal[_]](cvar : _*)
  def iterator = internalList.iterator
  def apply(idx: Int) = internalList(idx)
  def length = internalList.length
  def and( cvar : SVal[_] ) : SValSeq = { internalList = cvar :: internalList; this }
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

abstract class Semantics extends Serializable {
  def toSymbol: Symbol

  override def toString: String = toSymbol.name
  override def hashCode() = toSymbol.hashCode()
  override def equals(p1: Any) = p1 match {
    case sem : Semantics => sem.toSymbol.equals(toSymbol)
    case _ => false
  }
}

//abstract class SValSemantics(override val value: Semantics) extends SVal[Semantics](OntologySymbol)
