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

import simplex3d.math.floatx.ConstVec3f
import simx.core.entity.description.HistoryStorage.HistoryType
import simx.core.entity.description.SVal.SValType
import simx.core.svaractor.TimedRingBuffer.{Now, ContentType, Time}
import simx.core.svaractor.semantictrait.base._
import simx.core.svaractor.{AccessMethod, StateParticle, SVarActor, ImmutableSVar}
import simx.core.entity.typeconversion._
import simx.core.ontology._
import scala.reflect.ClassTag


object SVal{
  def apply[T : ClassTag, V <: T, X <: Base, S <: Thing]( semantics : TypeInfo[T,T], valueDescription: ValueDescription[X, S], timestamp : Long = -1, history: HistoryStorage.HistoryType[T] = Nil)( value : V ) : SVal[T,TypeInfo[T,T], X, S] =
    new SVal[T,TypeInfo[T,T], X, S] (value, valueDescription, semantics, timestamp, history)

//  def applyAs[T : ClassTag, V <: T, TType <: TypeInfo[T,T], X <: Base, S <: Thing]( semantics : TType )( value : V ) : SVal[T,TType, X, S] =
//    new SVal[T,TType, X, S] (semantics)(value)

  type SValType[T] = SVal[T,TypeInfo[T,T], _ <: Base, _ <: Thing ]
}

case class InterpolationException(message: String) extends Exception(message)

trait Interpolator[DataType, S <: Thing]{

  /**
   * Returns an interpolated value for a given timestamp at, based on a history of values seq
   * @param seq A sequence of value-timestamp pairs that are assumed to be already sorted with respect to their timestamps (newer values before older ones)
   * @param at The timestamp for which a value is to be interpolated.
   */
  def apply(seq : Seq[(DataType, Long)], at : Long) : DataType
}

trait LinearInterpolator[DataType,  S <: Thing]
  extends Interpolator[DataType, S]
{
  /**
   * Interpolates linearly between newer and older, based on ratio. The following boundary conditions have to be met:
   * Ratio = 0 has result in newer
   * Ratio = 1 has result in older
   */
  def interpolate(newer: DataType, older: DataType, ratio: Float): DataType

  final def apply(seq: Seq[(DataType, Long)], at : Long): DataType = {
//    println(at, seq.map(_._2))
    if (seq.head._2 > at) {
      if (seq.tail.isEmpty) {
        throw new InterpolationException("[LinearInterpolator][" + seq.head._1.getClass.getCanonicalName + "] Passed timestamp is older than the oldest available value.")
      }
      else if (seq.tail.head._2 > at)
        apply(seq.tail, at)
      else {
        val ratio = (seq.head._2 - at).toFloat / (seq.head._2 - seq.tail.head._2).toFloat
        interpolate(seq.head._1, seq.tail.head._1, ratio)
      }
    }
    else if (seq.head._2 == at) {
      seq.head._1
    }
    else {
      throw new InterpolationException("[LinearInterpolator][" + seq.head._1.getClass.getCanonicalName + "] Passed timestamp is newer than the latest available value.")
    }
  }

  final def seek(seq : Seq[(DataType, Long)], to : Long) : Seq[(DataType, Long)] = {
    if (seq.head._2 > to) {
      if (seq.tail.isEmpty)
        seq.tail
      else if (seq.tail.head._2 > to)
        seek(seq.tail, to)
      else
        seq
    } else {
      Seq[(DataType, Long)]()
    }
  }
}

case class SyncTime(var t0: Long)

object HistoryStorage {
  type HistoryType[T] = List[(T, Long)]
  val maxHistorySize = 600
}

trait HistoryStorage[T] {
  def typedSemantics : TypeInfo[T, T]
  def value : T

  //protected var DefaultInterpolator : Option[Interpolator[T]] = None
  protected val timestamp : Long

  private lazy val constructor =
    getClass.getConstructor(typedSemantics.classTag.runtimeClass, classOf[scala.Long], classOf[HistoryStorage.HistoryType[T]])
  protected val history : HistoryType[T]

  private lazy val cs = getClass.getConstructors

  protected def newInstance(value : T) : this.type = newInstance(value, -1L, Nil)
  
  protected def newInstance(value : T, timestamp: Long, history:  HistoryType[T]) : this.type =
    constructor.newInstance(value.asInstanceOf[Object], timestamp.asInstanceOf[Object], history.asInstanceOf[Object]).asInstanceOf[this.type]

  def getC = getClass.getConstructors

  def getTimeStamp = timestamp
  def getHistory = history

  def withHistoryPrependedBy(value : T, timestamp : Long ) : this.type = {
    var newHistory = (this.value, this.timestamp) :: getHistory
    if(newHistory.size > HistoryStorage.maxHistorySize) newHistory = newHistory.dropRight(newHistory.size - HistoryStorage.maxHistorySize)
    try {
      newInstance(value, timestamp, newHistory)
    } catch {
      case _: NoSuchMethodException =>
        this match {
          case sVal: SVal[_,_,_,_] =>
            getClass.getConstructors.apply(0).newInstance(value.asInstanceOf[Object], sVal.valueDescription.asInstanceOf[Object], sVal.typedSemantics.asInstanceOf[Object], timestamp.asInstanceOf[Object], newHistory.asInstanceOf[Object]).asInstanceOf[this.type]
          case _: Throwable => throw new Exception("***")
        }
      case _: Throwable => throw new Exception("***")
    }
  }

  override def toString: String =
    super.toString + (if(history.nonEmpty) " with history " + history.map(_._2).mkString("-:-") else "")
}

case class SValHistoryException(msg: String) extends Exception(msg)

trait SValHistory[T, S <: Thing, NonHistoryType <: SVal.SValType[T]] extends HistoryStorage[T] {

  def newNonHistoryInstance(value : T): NonHistoryType

  def at(t : Long)(implicit interpolator : Interpolator[T,S]) : NonHistoryType = {
    try {
      newNonHistoryInstance(interpolator((this.value, this.timestamp) :: getHistory, t))
    } catch {
      case e: InterpolationException =>
        val oldestValue = getHistory.sortWith(_._2 < _._2).headOption.map(_._2)
        val difference = oldestValue.map(t - _)
        val details = if(difference.isDefined) "The requested timestamp is " + difference.get + "ms older than the oldest available value." else "No history values available at all."
        throw SValHistoryException("Could not access value in SValHistory of SVal " + typedSemantics.semantics.toString + ": " + details)
    }
  }

  def at(t: simx.core.ontology.types.Milliseconds)(implicit sync: SyncTime, interpolator : Interpolator[T,S]): NonHistoryType =
    at(sync.t0 - t.value)(interpolator)
}

class SVal[T, +TType <: TypeInfo[T,T], +X <: Base, +S <: Thing](val value : T, val valueDescription: ValueDescription[X, S], val typedSemantics : TypeInfo[T, T], protected val timestamp: Long, protected val history : HistoryStorage.HistoryType[T])
  extends SValBase[T, T] with SemanticValue[T, S] with HistoryStorage[T] with StateParticle[T] with Serializable
{
  def asSVal : SVal[T, TypeInfo[T,T], X, S] =
    this

  def add(newValue: T): this.type =
    getClass.getConstructor(typedSemantics.classTag.runtimeClass).newInstance(newValue.asInstanceOf[java.lang.Object]).asInstanceOf[this.type]

  def typedValue: T =
    value

  def asConvertibleTrait: ConvertibleTrait[T] =
    typedSemantics.asConvertibleTrait

  def as(gs : GroundedSymbol) =
    SVal(typedSemantics.asConvertibleTrait.addAnnotations(gs), valueDescription)(value)(typedSemantics.classTag)
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
sealed trait SValBase[+T, B <: T] extends ImmutableSVar[T, B] with Serializable
{
  def asSVal : SVal[B,TypeInfo[B,B], _ <: Base, _ <: Thing]
  val typedSemantics: TypeInfo[T, B]
  val value: T

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

  def and(that : SVal[_,_ <: TypeInfo[_, _], _ <: Base, _<: Thing]) : SValSet =
    new SValSet(asSVal, that)

//  def and : Seq[SVal[_,_ <: TypeInfo[_, _], _ <: Base, _<: Thing]] => SValSeq =
//    _.foldLeft(new SValSeq)( _ and _ )
}

//class SValSeq ( cvar : SVal[_,_, _ <: Base, _<: Thing]* ) extends Seq[SVal[_,_, _ <: Base, _<: Thing]]{
//  private var internalList = List[SVal[_,_, _ <: Base, _<: Thing]](cvar : _*)
//  def iterator = internalList.iterator
//  //def apply(idx: Int) = internalList(idx)
//
//  override def apply(idx: Int): SVal[_, _, _ <: Base, _<: Thing] = internalList(idx)
//
//  def length = internalList.length
//  def and( cvar : SVal[_,_ <: TypeInfo[_, _], _ <: Base, _<: Thing] ) : SValSeq = { internalList = cvar :: internalList; this }
//}


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
