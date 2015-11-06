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

package simx.core.svaractor.unifiedaccess

import simx.core.ontology
import simx.core.ontology.Annotation
import simx.core.svaractor.TimedRingBuffer.{Now, Time, BufferMode}
import simx.core.svaractor._
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.description.SValBase

/**
 * Created by dennis on 24.04.14.
 *
 */
/**
 * @param identifier the sVarIdentifier
 * @param annotations the stateparticles annotations
 * @param svar the state particle itselt
 * @param typeInfo a convertible trait describing the state particle. This may not contain annotations and probably refers to its base type
 * @tparam T the state particles type
 */
case class StateParticleInfo[T](identifier : Symbol, annotations : Set[Annotation], svar : StateParticle[T], typeInfo : ConvertibleTrait[T]) {
  def matches[U](c: ConvertibleTrait[U]): Boolean = {
    c.getBase == typeInfo.getBase &&
    c.sVarIdentifier == identifier &&
    c.annotations == annotations
  }
}

trait StateParticleAccess extends UnifiedAccess{
  /**
   *
   * @param c the convertable trait describing the state particle to be accessed
   * @param actorContext the actor context in which the access is executed
   * @param filter an optional filter function that may be used to filter the accessed state particle
   * @tparam T the feature's type
   * @return a method that itself takes a handler as a parameter which operates on the looked up annotated map
   */
  protected def access[T](c : ConvertibleTrait[T], actorContext : EntityUpdateHandling,
                          filter : StateParticle[T] => Boolean = (_ : StateParticle[T]) => true ) : (AnnotatedMap[T] => Any) => Unit


  protected def getSVars[T](out : ConvertibleTrait[T])(implicit context : EntityUpdateHandling) : List[(Set[ontology.Annotation], StateParticle[T])]

  /**
   *
   * @param c the newly added value
   * @param handler a handler function which operates on the new representation of "this"
   * @param actor the actor context in which the handler is executed
   * @tparam T the added state particles type
   */
  protected def handleNewValue[T](c : SValBase[T, _], timeStamp : Time, handler : SelfType => Any, bufferMode : BufferMode)
                                 (implicit actor : EntityUpdateHandling)

  /**
   *
   * @param handler a handler that is executed each time "this" is updated
   * @param actorContext the actor context in which the handler is executed
   * @return an id that can be used to unregister the update handler
   */
  def onUpdate(handler : SelfType => Any)(implicit actorContext : EntityUpdateHandling) : java.util.UUID

  /**
   *
   * @param id the id returned by onUpdate to identify the handler to be unregistered
   * @param actorContext the actor context in which the method call is executed
   */
  def ignore(id : java.util.UUID)(implicit actorContext : EntityUpdateHandling)

  /**
   * @param context the actor context in which the method call is executed
   * @return an iterable of [[simx.core.svaractor.unifiedaccess.StateParticleInfo]]'s describing all contained state particles
   */
  def getAllStateParticles(implicit context : EntityUpdateHandling) : Iterable[StateParticleInfo[_]]

  /**
   * @param c the convertible trait describing the state particle to be removed
   * @param handler an optional function that will be called with the updated value of "this"
   * @param actor the actor context in which the handler will be executed
   * @tparam T the type of the state particle to be removed
   */
  def remove[T](c : ConvertibleTrait[T], handler : SelfType => Any = _ => {})(implicit actor : EntityUpdateHandling)

  /**
   * @param c1 the first convertible trait describing a state particle to be observed
   * @param c2 the second convertible trait describing a state particle to be observed
   * @param cs further convertible traits describing a state particle to be observed
   * @param context the context of an actor mixing in the [[MultiObserve]] trait in which the handler will be executed
   * @return
   */
  final def observe(c1 : ConvertibleTrait[_], c2 : ConvertibleTrait[_], cs : ConvertibleTrait[_]*)(implicit context : MultiObserve) =
    context.observe(this, (c1 :: c2 :: cs.toList).toSeq :_*)

  /**
   *
   * @param c the convertible trait describing the state particle to be observed
   * @param ignoredWriters a optional set of actors whose updates will be ignored (often used for "self")
   * @tparam T the type of the state particle to be observed
   * @return an [[ObservableAccessSet]] on which handlers can be registered to access the defined state particles
   */
  final def observe[T](c : ConvertibleTrait[T], ignoredWriters : Set[SVarActor.Ref] = Set()) =
    new ObservableAccessSet[T, T](this, access(c, _), _.observe(ignoredWriters), x => x)

  /**
   *
   * @param c the convertible trait describing the state particle to be accessed
   * @tparam T the type of the state particle to be accessed
   * @return an [[AccessSet]] on which handlers can be registered to access the defined state particles
   */
  final def get[T](c : ConvertibleTrait[T], at : Time, accessMethod : AccessMethod) : AccessSet[T, T]=
    new AccessSet[T, T](access(c, _), _.accessValue(at, accessMethod), x => x )

  final def get[T](c : ConvertibleTrait[T], at : Time) : AccessSet[T, T] =
    get(c, at, GetClosest)

  final def get[T](c : ConvertibleTrait[T]) : AccessSet[T, T] =
    get(c, Now)

  /**
   *
   * @param c the convertible trait describing the state particle to be updated (or inseted if it does not exist)
   * @param handler an optional handler that is executed after the value was inserted
   * @param actorContext the actor context in which the handler is executed
   * @tparam T the inserted state particles type
   * @throws AmbiguousSelectionException if there are multiple state particles matching c
   */
  final def set[T](c : SValBase[T, _ <: T], timeStamp : Time, bufferMode : BufferMode)
                  (handler : SelfType => Any)
                  (implicit actorContext : EntityUpdateHandling) {
    getSVars(c.typedSemantics.asConvertibleTrait) match {
      case (_, stateparticle : StateParticle[T@unchecked]) :: Nil =>
        stateparticle.set(c.value, timeStamp)
        handler(asSelfType)
      case _ => access(c.typedSemantics.asConvertibleTrait, actorContext ) { toSet =>
        toSet.filter(_._1 equals AnnotationSet(c.typedSemantics.annotations.toSeq: _*)) match {
          case set if set.size < 2 =>
            handleNewValue(c, timeStamp, handler, bufferMode)
          case set =>
            throw AmbiguousSelectionException(set, c)
        }
      }
    }
  }

  final def set[T](c : SValBase[T, _ <: T], bufferMode : BufferMode)(handler : SelfType => Any)(implicit actorContext : EntityUpdateHandling) {
    set(c, Now, bufferMode)(handler)
  }

  final def set[T](c : SValBase[T, _ <: T], bufferMode : BufferMode)(implicit actorContext : EntityUpdateHandling) {
    set(c, Now, bufferMode)((_: SelfType) => {})
  }

  final def set[T](c : SValBase[T, _ <: T], handler : SelfType => Any = (_ : SelfType) => {})(implicit actorContext : EntityUpdateHandling) {
    set(c, Now, TimedRingBuffer.defaultMode)(handler)
  }
}

case class AmbiguousSelectionException[T](set : AnnotatedMap[_], c : SValBase[T, _ <: T])
  extends Exception("Ambiguous selection of StateParticles in 'set': " + set + " when asking for " + c.typedSemantics)
