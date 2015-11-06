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

import java.util.UUID

import simx.core.svaractor.TimedRingBuffer.{BufferMode, Time}
import simx.core.svaractor.{RemoveEntityMessage, SVar, SVarActor}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.description.SValBase
import simx.core.entity.Entity
import scala.collection.mutable

/**
 * Created by dennis on 24.04.14.
 *
 */
trait EntityUpdateHandling extends SVarActor{
  protected implicit def arrowAssocToRightUnknownTuple[T](tuple : (T, Unknown)) : RightUnknownTuple[T] =
    RightUnknownTuple(tuple._1)

  addHandler[HandleEntityUpdate[_]]{
    msg => delayedReplyWith[Entity](msg.e.set(msg.sval, msg.timeStamp, msg.bufferMode))()
  }

  addHandler[HandleEntityRemove[_]]{
    msg => delayedReplyWith[Entity](msg.e.remove(msg.sval, _))()
  }

  addHandler[HandleObserverUpdate]{
    msg => delayedReplyWith[Entity](msg.e.addRemoveObservers(msg.newObservers, _))()
  }

  addHandler[RemoveEntityMessage]{ msg =>
    removeUpdaters(msg.e)
    removeFromLocalRep(msg.e)
  }

  addHandler[HandleRemoveEntity]{
    msg => msg.e.remove(); msg.e
  }

  protected def removeFromLocalRep(e : Entity)

  private val eRefs = mutable.Map[java.util.UUID, Entity]()
  private val observedEntities = mutable.Map[Entity, java.util.UUID]()
  private val onUpdates = mutable.Map[java.util.UUID, Map[java.util.UUID, Entity => Any]]()
  private val storedIds = mutable.Map[Any, java.util.UUID]()

  private def removeUpdaters(e : Entity){
    observedEntities remove e collect{
      case svarId =>
        e.getAllStateParticles.map(_.svar.ignore())
        eRefs.remove(svarId)
        onUpdates.remove(svarId)
        e.ignore()
    }
    eRefs.retain((a, b) => b != e)
  }

  protected[core] def addInternalUpdater(e : Entity, ref : SVar[Option[Entity]], handler : Entity => Any) = {
    val id = java.util.UUID.randomUUID()
    onUpdates.update(ref.id, onUpdates.getOrElse(ref.id, Map()).updated(id, handler))
    if (!observedEntities.contains(e))
      updateObserve(e, ref)
    id
  }

  protected[core] def storeUpdateId(id : java.util.UUID, ref : Any){
    storedIds.update(ref, id)
  }

  protected[core] def getUpdateId(ref : Any) =
    storedIds.get(ref)


  protected[core] def removeInternalUpdater(ref : SVar[Option[Entity]], id : java.util.UUID){
    onUpdates.update(ref.id, onUpdates.getOrElse(ref.id, Map()) - id)
  }

  protected[core] def updateInternalRep[T <: Entity](id : UUID, newVal : T) {
    eRefs.update(id, newVal)
  }

  protected[core] def get(ref : SVar[Option[Entity]]) =
    eRefs.get(ref.id)

  protected def get(e : Entity) =
    eRefs.find(_._2 == e).collect{ case (_, entity) => entity }

  protected[core] def updateObserve[T <: Entity](e : Entity, ref : SVar[Option[T]], handler : T => Any = (_ : T) => {}) = {
    observedEntities.update(e, ref.id)
    ref.observe({ newVal =>
      if (newVal.isDefined) {
        eRefs.update(ref.id, newVal.get)
        onUpdates.getOrElse(ref.id, Map()).foreach(_._2.apply(newVal.get))
        handler(newVal.get)
      }
    }, Set())
  }
}

protected[core] case class HandleRemoveEntity(e : Entity)
protected[core] case class HandleEntityUpdate[T](e : Entity, sval : SValBase[T, _ <: T], timeStamp : Time, bufferMode : BufferMode)
protected[core] case class HandleEntityRemove[T](e : Entity, sval : ConvertibleTrait[T])
protected[core] case class HandleObserverUpdate(e : Entity, newObservers : Set[SVarActor.Ref])
