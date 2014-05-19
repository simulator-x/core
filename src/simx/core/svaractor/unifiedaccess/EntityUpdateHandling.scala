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

import simx.core.svaractor.{RemoveEntityMessage, SVar, SVarActor}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.description.SVal
import simx.core.entity.Entity

/**
 * Created by dennis on 24.04.14.
 *
 */
trait EntityUpdateHandling extends SVarActor{
  addHandler[HandleEntityUpdate[_]]{
    msg => delayedReplyWith[Entity](msg.e.set(msg.sval, _))()
  }

  addHandler[HandleEntityRemove[_]]{
    msg => delayedReplyWith[Entity](msg.e.remove(msg.sval, _))()
  }

  addHandler[HandleObserverUpdate]{
    msg => delayedReplyWith[Entity](msg.e.addRemoveObservers(msg.newObservers, _))()
  }

  addHandler[RemoveEntityMessage]{
    msg => removeFromLocalRep(msg.e)
  }

  addHandler[HandleRemoveEntity]{
    msg => msg.e.remove(); msg.e
  }

  protected def removeFromLocalRep(e : Entity)

  private var eRefs = Map[java.util.UUID, Entity]()
  private var observedEntities = Set[java.util.UUID]()
  private var onUpdates = Map[java.util.UUID, Map[java.util.UUID, StateParticleAccess => Any]]()

  protected[core] def addInternalUpdater(ref : SVar[Entity], handler : StateParticleAccess => Any) = {
    val id = java.util.UUID.randomUUID()
    onUpdates = onUpdates.updated(ref.id, onUpdates.getOrElse(ref.id, Map()).updated(id, handler))
    if (!observedEntities.contains(ref.id))
      updateObserve(ref, Set())
    id
  }

  protected[core] def removeInternalUpdater(ref : SVar[Entity], id : java.util.UUID){
    onUpdates = onUpdates.updated(ref.id, onUpdates.getOrElse(ref.id, Map()) - id)
  }

  protected[core] def get(ref : SVar[Entity]) =
    eRefs.get(ref.id)

  protected def get(e : Entity) =
    eRefs.find(_._2 == e).collect{ case (_, entity) => entity }

  protected[core] def updateObserve(ref : SVar[Entity], handler : Entity => Any = _ => {}) = {
    observedEntities = observedEntities + ref.id
    ref.observe({ newVal =>
      eRefs = eRefs.updated(ref.id, newVal)
      onUpdates.getOrElse(ref.id, Map()).foreach(_._2.apply(newVal))
      handler(newVal)
    }, Set())
  }
}

protected[core] case class HandleRemoveEntity(e : Entity)
protected[core] case class HandleEntityUpdate[T](e : Entity, sval : SVal[T])
protected[core] case class HandleEntityRemove[T](e : Entity, sval : ConvertibleTrait[T])
protected[core] case class HandleObserverUpdate(e : Entity, newObservers : Set[SVarActor.Ref])
