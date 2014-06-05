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

package simx.core.svaractor

import simx.core.svaractor.synclayer.AtomicSetSupport
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.svaractor.unifiedaccess.{StateParticleAccess, EntityUpdateHandling, AnnotationSet}
import simx.core.svaractor.ExtensibleObserve.HandlerType

/**
 * Created by dwiebusch on 09.05.14
 */
trait MultiObserve extends EntityUpdateHandling with AtomicSetSupport{
  private var updateHandlers = Map[SVar[_], Map[java.util.UUID, Any => Unit]]()
  private var reverseHandlerLookup = Map[java.util.UUID, SVar[_]]()

  protected[core] def observe(e : StateParticleAccess, cs: ConvertibleTrait[_]*) =
    new ExtensibleObserve(e, cs :_*)

  protected[svaractor] def ignore(handlerId : java.util.UUID){
    reverseHandlerLookup.get(handlerId).collect{
      case svar =>
        val newHandlerMap    = updateHandlers.getOrElse(svar, Map()) - handlerId
        updateHandlers       = updateHandlers.updated(svar, newHandlerMap)
        reverseHandlerLookup = reverseHandlerLookup - handlerId
        if (newHandlerMap.nonEmpty) {
          val newHandlers = newHandlerMap.values
          sVarObserveHandlers.update(svar, value => newHandlers.foreach(_.apply(value)))
        } else
          sVarObserveHandlers.remove(svar)
    }
  }

  override private[svaractor] def addSVarObserveHandler[T](svar : SVar[T])(handler : T => Unit ) = {
    val id = java.util.UUID.randomUUID()
    val newHandlers      = updateHandlers.getOrElse(svar, Map()).updated(id, handler.asInstanceOf[Any => Unit])
    updateHandlers       = updateHandlers.updated(svar, newHandlers)
    reverseHandlerLookup = reverseHandlerLookup.updated(id, svar)
    val newHandlerFuncs  = newHandlers.values
    super.addSVarObserveHandler(svar)( value => newHandlerFuncs.foreach(_.apply(value)) )
    id
  }

  private[svaractor] def afterAllUpdates(func : => Unit){
    doAfterAtomicSet(func)
  }
}

object ExtensibleObserve{
  type HandlerType = (InnerSet, Set[(StateParticleAccess, ConvertibleTrait[_], AnnotationSet)]) => Unit
}

class ExtensibleObserve private(private val toObserve : Map[StateParticleAccess, Set[ConvertibleTrait[_]]],
                                currentHandler : HandlerType)
{
  def this(toObserve: Map[StateParticleAccess, Set[ConvertibleTrait[_]]]) =
    this(toObserve, (_, _) => {})

  def this(e: StateParticleAccess, cs: ConvertibleTrait[_]*) = this(Map(e -> cs.toSet))

  def and(e: StateParticleAccess, cs: ConvertibleTrait[_]*) : ExtensibleObserve =
    new ExtensibleObserve(toObserve.updated(e, toObserve.getOrElse(e, Set()) ++ cs.toSet))

  def and(that : ExtensibleObserve) : ExtensibleObserve =
    new ExtensibleObserve(that.toObserve.foldLeft(toObserve){
      (map, entry) => map.updated(entry._1, map.getOrElse(entry._1, Set()) ++ entry._2)
    })

  def onUpdate(handler: HandlerType)(implicit actor: MultiObserve) {
    new ExtensibleObserve(toObserve, handler).initObserving(actor)
  }

  def ignore()(implicit actor : MultiObserve) {
    handlerIds.foreach(actor.ignore)
  }

  private var handlerIds =
    Set[java.util.UUID]()

  private val is =
    new InnerSet()

  private def initObserving(actor: MultiObserve) {
    toObserve.map { entry => entry._2.map(singleObserve(entry._1, _, actor))}
  }

  private def singleObserve[T](entity: StateParticleAccess, c: ConvertibleTrait[T], actor: MultiObserve) {
    entity.observe(c).forall {
      _.map { tuple =>
        handlerIds = handlerIds + tuple._2.observe { change =>
          is.updateValue(entity, tuple._1.set, c -> change)
          actor.afterAllUpdates{ if (is.hasUpdates) currentHandler.apply(is, is.getUpdatedValues) }
        }(actor)
      }
    }(actor)
  }
}

class InnerSet private[svaractor]{
  private type ValueTuple[T] = (ConvertibleTrait[T], T)
  private var updatedValues = Set[(StateParticleAccess, ConvertibleTrait[_], AnnotationSet)]()
  private var values : Map[ConvertibleTrait[_], Map[AnnotationSet, ValueTuple[_]]] = Map()

  private def convertTuple[T, U](tuple : ValueTuple[T], to : ConvertibleTrait[U]) : U =
    tuple._1.convert(to)(tuple._2)

  def updateValue[T](e : StateParticleAccess, sp : AnnotationSet, tuple : ValueTuple[T]){
    values = values.updated(tuple._1, values.getOrElse(tuple._1, Map()).updated(sp, tuple))
    updatedValues = updatedValues + ((e, tuple._1, sp))
  }

  def hasUpdates =
    updatedValues.nonEmpty

  def getUpdatedValues = {
    val retVal = updatedValues
    updatedValues = Set()
    retVal
  }

  def get[T](c : ConvertibleTrait[T]) : Map[AnnotationSet, T] =
    values.get(c) match {
      case Some(map) => map.map( tuple => tuple._1 -> convertTuple(tuple._2, c) )
      case None => Map()
    }
}