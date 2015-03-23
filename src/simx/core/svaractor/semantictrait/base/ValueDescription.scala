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

import simx.core.ontology.Annotation

/**
 * Created by dwiebusch on 30.11.14
 */

trait ValueDescriptionBase[+B <: Base, +S <: Thing] extends Base with Serializable{
  val groundedSymbol : GroundedSymbolBase[S]
  val annotations : Set[Annotation]
  val base : B

  def subsumes(that : Base) : Boolean = {
    if (that == this) true
    else if (that.base == Base) false
    else subsumes(that.base)
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case that :  ValueDescriptionBase[_, _] => groundedSymbol ==
      that.groundedSymbol && annotations == that.annotations
    case _ => false
  }

  override def hashCode(): Int =
    groundedSymbol.hashCode() + 41 * annotations.hashCode()
}

sealed trait Base extends Serializable {
  val base : Base
}

private object Base extends Base with Serializable{
  override val base: Base = this
}

private[core] object BaseValueDescription{
  def apply[S <: GroundedSymbolFeatures](gsym: S, annotations : Set[Annotation] = Set()) =
    apply[S#SymbolType](gsym.Symbol, annotations)

  def apply[S <: Thing](gsym : GroundedSymbolBase[S]) =
    new ValueDescription[Base, S](Base, gsym, Set())

  def apply[S <: Thing](gsym : GroundedSymbolBase[S], annotations : Set[Annotation]) =
    new ValueDescription[Base, S](Base, gsym, annotations)
}

sealed class ValueDescription[+B <: Base, +S <: Thing] (val base : B,
                                                        val groundedSymbol : GroundedSymbolBase[S],
                                                        val annotations : Set[Annotation])
  extends ValueDescriptionBase[B, S] with Serializable
{
  override def toString: String =
    groundedSymbol.toString + (if (annotations.nonEmpty) "@(" + annotations.mkString(", ") + ")" else "")

  def setAnnotations(newAnnotations: Set[Annotation]) =
    new ValueDescription[B, S](base, groundedSymbol, newAnnotations)

  def extendBy[T <: Thing](thatSymbol: GroundedSymbolBase[T]) : ValueDescription[B, S with T] =
    new ValueDescription(base, groundedSymbol.and[T](thatSymbol), annotations)

  def as[T <: GroundedSymbolFeatures](thatSymbol : T) : ValueDescription[ValueDescription[B, S], T#SymbolType] =
    new ValueDescription(this, thatSymbol.Symbol, annotations)
}


