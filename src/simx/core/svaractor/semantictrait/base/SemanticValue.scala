/*
 * Copyright 2014 The SIRIS Project
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

package simx.core.svaractor.semantictrait.base

import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

import scala.util.continuations


/**
 * Created by dwiebusch on 30.11.14
 */
trait SemanticValue[+T, +B <: Thing] extends Attain with Serializable{
  val valueDescription :  ValueDescription[_ <: Base, B]
  val value : T

  override def toString: String =
    valueDescription  + "(" + value + ")"

  def set(r : Seq[Semantic.Value[Any]])(implicit context: EntityUpdateHandling) : Unit =
    r.foreach(ABox.set(this, _)( _ => () ))

  def modify[C <: Thing](r: SemanticValue[_, C])(implicit context: EntityUpdateHandling) : Settable[T, B with C] =
    Settable(ABox.set[T, B, C](this, r))

  def set[C <: Thing](r: SemanticValue[_, C])(implicit context: EntityUpdateHandling) : SemanticValue[T, B with C] @CPSRet =
    continuations.shift{ k : (SemanticValue[T, B with C] => Any) => modify(r)(context)(s => k(s) : Unit) }

  def modify[U >: T, C <: Thing](r :  PartialRelation[U, _, C])(implicit context: EntityUpdateHandling) : Settable[T, B with C]=
    Settable(ABox.set[T, B, C](this, r.apply(this)))

  def set[U >: T, C <: Thing](r :  PartialRelation[U, _, C])(implicit context: EntityUpdateHandling) : SemanticValue[T, B with C] @CPSRet =
    continuations.shift{ k : (SemanticValue[T, B with C] => Any) => modify(r)(context)(s => k(s) : Unit) }

  def get[V](v : Semantic.Value[V])(implicit context: EntityUpdateHandling) : Iterable[Semantic.Value[_]] @CPSRet =
    continuations.shift{ k : (Iterable[Semantic.Value[_]] => Any) => handleValue(v)(s => k(s)) }

  def handleValue[V](v : Semantic.Value[V])(handler : Iterable[Semantic.Value[_]] => Unit)(implicit context: EntityUpdateHandling) : Unit =
    ABox.get[Semantic.Value[_]](this, v)(handler)

  def matches(that : SemanticValue[_, _ <: Thing])(implicit context: EntityUpdateHandling) : Boolean@CPSRet =
    ABox.matches(this, that)


  override def equals(obj: scala.Any): Boolean = obj match {
    case that : Semantic.Value[_] => value == that.value && valueDescription == that.valueDescription
    case _ => false
  }

  override def hashCode(): Int =
    valueDescription.hashCode()

  def subsumes(that : Semantic.Value[_])(implicit context: EntityUpdateHandling) : Boolean@CPSRet =
    if(valueDescription subsumes that.valueDescription){
      if (value == this) true
      else value == that.value
    } else
      false
}

trait Attain extends Serializable{
  def attain(v : Semantic.Value[_]*)(implicit context: EntityUpdateHandling) = {
    Action.lookup(v.toSet) match {
      case Some(action) =>
        val params = action.matches(v.toSet)
        if (params.nonEmpty)
          Some(action.apply(params.get.toArray.sortWith(_._1 < _._1).map(_._2)))
        else
          None
      case None => None
    }
  }

  def attainR[T, B <: Base, S <: Thing](retTyp : SemanticTypeTrait[T, B, S], v : Semantic.Value[_]*)(implicit context: EntityUpdateHandling) : Option[SemanticValue[T, S]]@CPSRet  = {
    Action.lookup(v.toSet, retTyp) match {
      case Some(action ) =>
        val params = action.matches(v.toSet)
        if (params.nonEmpty)
          Some(action.apply(params.get.toArray.sortWith(_._1 < _._1).map(_._2)))
        else
          None
      case None => None
    }
  }
}

case class Settable[+T, +B <: Thing](accessValue : (SemanticValue[T, B] => Unit) => Unit){
  def apply(nextHandler : SemanticValue[T, B] => Unit = _ => ()){
    accessValue(v => nextHandler(v))
  }

  def set : SemanticValue[T, B] @CPSRet =
    continuations.shift{ k : (SemanticValue[T, B] => Any) => apply(s => k(s) : Unit) }

  def set[C <: Thing](r: SemanticValue[_, C])(implicit context: EntityUpdateHandling) : Settable[T, B with C] =
    Settable[T, B with C](accessNext => accessValue(_.modify(r).apply(accessNext)))

  def set[U >: T, C <: Thing](r :  PartialRelation[U, _, C ])(implicit context: EntityUpdateHandling) : Settable[T, B with C] =
    Settable[T, B with C](accessNext => accessValue(_.modify[U, C](r).apply(accessNext)))
}
