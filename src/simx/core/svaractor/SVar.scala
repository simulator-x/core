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

package simx.core.svaractor

import scala.reflect.runtime.universe.TypeTag
import java.util.UUID
import scala.reflect.ClassTag
import scala.util.continuations
import simx.core.entity.description.{SValBase, SVal}
import simx.core.entity.typeconversion.{ConvertedSVar, ConversionInfo}
import unifiedaccess.{Observability, Mutability, Immutability, Accessibility}
import simx.core.svaractor.SVarActor.Ref


// Static methods for SVars.
/**
 * This is the companion object of the SVar trait.
 *
 * It has two methods to create a new State Variables.
 */
trait SVarObjectInterface {
  // Create a new SVar with initial value and owner.
  /**
   * This method creates a new State Variable at the given actor.
   * The State Variable will have the given value and will have
   * the given and not changeable datatype of the given value.
   * The method blocks until the state variable can be provided.
   *
   *
   * @ param value The initial value of the State Variable
   * @ param owner The owner of the state variable. Must not be null.
   * @ tparam T The datatype of the state variable.
   * @ return The created state variable. Never returns null.
   */
//  def apply[T](value: T, owner: SVarActor.Ref)(implicit actorContext : SVarActor) : SVar[T]

  // Create a new SVar with initial value and self. Fails if self is not an SVarActor.
  // throws NotSVarActorException
  /**
   * This method creates a new state variable on the calling actor. This
   * method must be called within a SVarActor, otherwise a NotSVarActorException is
   * thrown. The method blocks until the creation of the State Variable is completly
   * finished.
   *
   *
   * @param value The initial value of the State Variable
   * @tparam T The data type of the state variable
   * @return The created state variable. Never returns null.
   */
  def apply[T](value: SVal[T])(implicit actorContext : SVarActor, typeTag : ClassTag[T]) : SVar[T]
}

trait SVBase[+T, B <: T] extends Observability[T] with Accessibility[T] with Mutability[B]{
  def observe(handler: (T) => Unit, ignoredWriters: Set[Ref] = Set())(implicit actorContext: SVarActor) : java.util.UUID
  def containedValueManifest : ClassTag[_ <: T]
  def ignore()(implicit actorContext : SVarActor)
  def as[T2](cInfo : ConversionInfo[T2, B]) : StateParticle[T2]
  def isMutable : Boolean
  def read(implicit actorContext : SVarActor) : T @continuations.cpsParam[Unit, Unit] =
    continuations.shift { k : (T => Unit) => get(k) }
}

trait StateParticle[T] extends SVBase[T, T]

/**
 * This trait represents a state variable. State Variables
 * are every time created trough the mechanism of the companion object.
 *
 * @tparam T The datatype of the represented value.
 */
trait SVar[T] extends StateParticle[T]  with Serializable{
  /**
   * The unique id trough which a SVar is identified
   */
  def id: UUID

  def initialOwner : SVarActor.Ref

  /**
   * Overrides equals to only compare _id
   */
  final override def equals(other: Any) : Boolean = other match {
    case that: SVar[_] => id.equals(that.id)
    case _ => false
  }

  /**
   * Overrides hashCode to use the _id's hashCode method
   */
  final override def hashCode =
    id.hashCode

  def as[T2](cInfo: ConversionInfo[T2, T]) : SVar[T2] =
    new ConvertedSVar(this, cInfo)
}

trait ImmutableSVar[+T, B <: T] extends SVBase[T, B] with Immutability[B] with Serializable{
  def ignore()(implicit actorContext: SVarActor){}
  def observe(handler: (T) => Unit, ignoredWriters: Set[SVarActor.Ref] = Set())(implicit actorContext: SVarActor) =
    java.util.UUID.randomUUID()
}


