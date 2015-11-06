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

import simx.core.svaractor.semantictrait.base.{Thing, Base}
import simx.core.worldinterface.entity.filter.{SValEquals, EntityFilter}

import scala.collection.mutable
import simx.core.ontology.GroundedSymbol
import simx.core.entity.typeconversion.{Reverter, TypeInfo, ConvertibleTrait}

/**
 * @author dwiebusch
 * Date: 29.09.11
 * Time: 08:57
 */


object SValSet {

  /**
   *
   * Creates a new SValSet
   */
  def apply(params: SVal.SValType[_]*) = new SValSet(params:_*)

  /**
   *
   * Creates a new SValSet that is a duplicate of that
   */
  def apply(that: SValSet) = new SValSet(that)

  implicit def toSValSet(sval: SVal.SValType[_]): SValSet = SValSet(sval)

}

/**
 * Groups several [[SVal]]s.
 * @see EntityAspect
 */
//TODO: make constructors package private to force companion object usage
class TypedSValSet[U](params: SValBase[U, _ <: U]*) extends mutable.HashMap[Symbol, List[SValBase[U, _ <: U]]] {
  params.foreach(p => add(p))

  /**
   *
   * Creates a new SValSet that is a duplicate of that
   */
  def this(that: TypedSValSet[_ <: U]) = this(that.values.flatten.toSeq :_*)

  /**
   * Returns a *new* [[TypedSValSet]] that contains all [[SVal]]s of 'this' [[TypedSValSet]] set as well as the [[SVal]] 'anotherSVal'
   */
  def and(anotherSVal: SValBase[U, _ <: U]) = {
    val res = new TypedSValSet(anotherSVal)
    values.flatten.foreach(res.add)
    res
  }

  /**
   *
   * Returns a NEW SValSet that does not contain any OntologyMember of type typedSemantics.
   */
  def -[T](typedSemantics: ConvertibleTrait[T]) : this.type = {
    remove(typedSemantics.sVarIdentifier)
    this
  }

  def add(SVal : SValBase[U, _ <: U]) {
    try {
      update(SVal.typedSemantics.sVarIdentifier, SVal :: getOrElse(SVal.typedSemantics.sVarIdentifier, Nil))
    } catch {
      case npe: NullPointerException =>
        println(SVal, SVal.typedSemantics.getClass, SVal.typedSemantics.semantics)
    }
  }

  def +=[T <: U](SVal : SValBase[U, _ <: U]) : this.type = {
    add(SVal)
    this
  }

  def ++=(that : TypedSValSet[U]) : this.type = {
    that.foreach(_._2.foreach(p => add(p)))
    this
  }

  /**
   *
   * Returns a List of all included SVals that are of a specific OntologyMember type.
   */
  def getAllSValsFor[T <: U](typedSemantics: TypeInfo[T, T]): List[SVal.SValType[T]] = {
    //TODO: Implement annotations check more efficient
    val res = getOrElse(typedSemantics.sVarIdentifier, Nil).filter(sVal => {
      typedSemantics.annotations.diff(sVal.typedSemantics.annotations).isEmpty
    })
    val res2 = res.map {
      case sVal: SVal[_,_,_,_] if sVal.typedSemantics.typeTag.tpe <:< typedSemantics.typeTag.tpe =>
        val value: T = sVal as typedSemantics.asConvertibleTrait
        val timestamp: Long = sVal.getTimeStamp
        val typedSVal: SVal.SValType[T] = sVal.asInstanceOf[SVal.SValType[T]]
        typedSemantics.asConvertibleTrait.apply(value, timestamp, typedSVal.getHistory)
      case sVal =>
        typedSemantics.asConvertibleTrait(sVal as typedSemantics.asConvertibleTrait)
    }
    res2
  }

//  case sVal: HistoryStorage[_] =>
//  val value: T = sVal as typedSemantics.asConvertibleTrait
//  val timestamp: Long = sVal.getTimeStamp
//
//  val convertedHistory: List[(T, Long)] = sVal.getHistory.flatMap { tuple =>
//    tuple._1 match {
//      case v if typedSemantics.classTag.runtimeClass == v.getClass => Some(v.asInstanceOf[T], tuple._2)
//      case _ =>
//        println("[warn][TypedSValSet] ...")
//        None
//    }
//  }
//  typedSemantics.asConvertibleTrait.apply(value, timestamp, convertedHistory)
//  case sVal =>
//    typedSemantics.asConvertibleTrait(sVal as typedSemantics.asConvertibleTrait)
//  case (v: T, t: Long) => (v, t)
//  case (v: sVal.containedValueManifest, t: Long) => (Reverter(typedSemantics.asConvertibleTrait, sVal.typedSemantics.getBase).revert(v), t)
//  case _ => throw new Exception("[TypedSValSet] ...")

/**
   *
   * Returns an Option on the SVal of a specific OntologyMember type.
   * Returns the first SVal in the order of this SValSet if more than one is present.
   */
  def getFirstSValFor[T <: U](typedSemantics: TypeInfo[T, T]): Option[SVal.SValType[T]] =
    getAllSValsFor(typedSemantics).headOption

  /**
   *
   * Returns the SVal of a specific OntologyMember type.
   * If the requested SVal is not found a SValNotFound is thrown.
   * If more than one valid SVal is present, the first SVal in the order of this SValSet is returned.
   */
  def firstSValFor[T <: U](typedSemantics: TypeInfo[T, T]): SVal.SValType[T] =
    getFirstSValFor(typedSemantics).getOrElse( throw SValNotFound(typedSemantics.sVarIdentifier) )

  /**
   *
   * Tests if this SValSet contains at least one SVal of the given OntologyMember type.
   */
  def contains(typedSemantics: TypeInfo[_, _]) : Boolean =
    contains(typedSemantics.sVarIdentifier)

  /**
   *  Adds the given SVal if no other SVal of its OntologyMember type
   *        was contained before.
   */
  def addIfNew(sVal: SValBase[U, _ <: U]) {
    if (!contains(sVal.typedSemantics)) add(sVal)
  }

  def replaceAllWith(sVal: SVal.SValType[_ <: U]) {
    remove(sVal.typedSemantics.sVarIdentifier)
    add(sVal)
  }

  def update[SValT <: U](sVal : SVal.SValType[SValT]){
    val newEntry = sVal :: get(sVal.typedSemantics.sVarIdentifier).collect{
      case list => list.filter(_.typedSemantics.annotations.diff(sVal.typedSemantics.annotations).nonEmpty)
    }.getOrElse(Nil)
    update(sVal.typedSemantics.sVarIdentifier, newEntry)
  }

  def remove[SValT <: U](sVal : SVal.SValType[SValT]){
    update(sVal.typedSemantics.sVarIdentifier, get(sVal.typedSemantics.sVarIdentifier).collect{
      case list => list.filter(_.typedSemantics.annotations.diff(sVal.typedSemantics.annotations).nonEmpty)
    }.getOrElse(Nil))
    if (get(sVal.typedSemantics.sVarIdentifier).collect{case x => x.isEmpty} == Some(true))
      remove(sVal.typedSemantics.sVarIdentifier)
  }

  def toSValSeq =
    values.flatMap(_.map(_.asSVal)).toSeq

  /**
   *
   * Returns a List of all included values that are of a specific OntologyMember type.
   */
  def getAllValuesFor[T <: U](typedSemantics: TypeInfo[T, T]): List[T] =
    getAllSValsFor(typedSemantics).map(_.value)

  /**
   *
   * Returns an Option on the value of a specific OntologyMember type.
   * Returns the first value in the order of this SValSet if more than one is present.
   */
  def getFirstValueFor[T <: U](typedSemantics: ConvertibleTrait[T]): Option[T] =
    getAllValuesFor(typedSemantics).headOption

  /**
   *
   * Returns the value of a specific OntologyMember type.
   * If the requested OntologyMember type is not found defaultValue is returned.
   * If more than one valid value is present, the first value in the order of this SValSet is returned.
   */
  def getFirstValueForOrElse[T <: U](typedSemantics: ConvertibleTrait[T])(defaultValue: => T): T =
    getFirstValueFor(typedSemantics).getOrElse(defaultValue)

  /**
   *
   * Returns the value of a specific OntologyMember type.
   * If the requested SVal is not found a SValNotFound is thrown.
   * If more than one valid value is present, the first value in the order of this SValSet is returned.
   */
  def firstValueFor[T <: U](typedSemantics: ConvertibleTrait[T]): T =
    getFirstValueFor(typedSemantics).getOrElse( throw SValNotFound(typedSemantics.sVarIdentifier) )

  /**
   *
   * Determines if this SValSet contains at least one SVal for each Providing
   */
  def satisfies(p: Set[Providing]): Boolean =
    p.flatMap(_.objects).forall(p => contains(p))

  /**
   *
   * Inserts all CreateParms of that into this SValSet
   */
  def mergeWith(that: TypedSValSet[_ <: U]): this.type = {that.foreach(this.+=); this}

  /**
   *
   * Inserts all new SVals of that into this SValSet
   */
  def xMergeWith[T <: U](that: TypedSValSet[T]): this.type = {
    that.values.flatten.foreach(addIfNew)
    this
  }

  /**
   * Combines the given convertible trait with the first matching value from this SValSet.
   * @param c the ConvertibleTrait to be combined with a value
   * @return an option containing the created cvar, or None if no matching value was found
   */
  def combineWithValue[T <: U](c : ConvertibleTrait[T]) : List[SVal[T,TypeInfo[T,T],_,_]] =
    getAllValuesFor(c) map { value => c(value) }

  /**
   * Combines all convertibles with their matching values in this SValSet.
   * @param cs the convertible trait to be matched
   * @return a tuple consisting of an SValSet containing the created CVars and a set of convertible traits
   *         for which no value was found
   */
  def combineWithValues(cs: Set[ConvertibleTrait[_ <: U]]): (SValSet, Set[ConvertibleTrait[_ <: U]]) =
    cs.foldLeft((new SValSet, cs.empty)) {
      (tuple, toCombine) =>
        combineWithValue(toCombine) match {
          case a :: b => b.foldLeft(tuple._1 += a ){ _ += _} -> tuple._2
          case Nil => (tuple._1, tuple._2 + toCombine)
        }
    }

  /**
   * Returns an [[EntityFilter]] that matches entities
   * which possess properties that equal all [[SVal]]s in this set (using [[SValEquals]]).
   */
  def toFilter: EntityFilter =
    combineFilters(values.flatten.map(_.asSVal).map{sVal => SValEquals(sVal)}.toList)

  private def combineFilters(filters: List[EntityFilter]): EntityFilter = filters match {
    case head :: Nil => head
    case head :: tail => head and combineFilters(tail)
    case _ => throw new Exception("[error][SValSet] Cannot create filter from empty SValSet")
  }

  def toString(indent: Int, heading: String = "SValSet"): String = {
    var indentString = ""
    for(i <- 0 until indent) indentString += "\t"
    indentString + heading  + mkString("\n"+indentString+"\t", "\n"+indentString+"\t", "")
  }

  def toShortString =
    values.flatten.map{_.value.toString}.mkString("[",",","]")

  override def toString(): String = toString(0)

  override def equals(other: Any): Boolean =
    other match {
      case that: TypedSValSet[U] =>
        (that canEqual this) &&
          keySet == that.keySet &&
          keySet.forall{ k =>
            val thisValue = this(k)
            val thatValue = that(k)
            if(thisValue.size == thatValue.size)
              thisValue.forall{v =>
                thisValue.count(_ == v) == thatValue.count(_ == v)
              }
            else false
          }
      case _ => false
    }

  override def canEqual(other: Any): Boolean =
    other.isInstanceOf[TypedSValSet[U]]

  override def hashCode(): Int =
    keySet.foldRight(41 + keySet.hashCode()){(k, hashSoFar) => 41 * hashSoFar + apply(k).hashCode()}

}

/**
 *
 * Groups several SVals.
 * This is used in the entity creation process for example.
 *
 * @see EntityAspect, SValSet
 */
final case class NamedSValSet(semantics: GroundedSymbol, ps: SVal.SValType[_]*) extends SValSet(ps: _*) {
  def this(that: NamedSValSet) = this(that.semantics, that.toSValSeq:_*)
  def this(aspectType: GroundedSymbol, cps: SValSet) = this(aspectType, cps.toSValSeq:_*)


  override def toString(): String =
    "NamedSValList (" + semantics + ")\n\t" + mkString("\n\t")
}