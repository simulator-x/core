/*
 * Copyright 2015 The SIRIS Project
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

package simx.core.worldinterface.entity

import java.util.UUID

import simx.core.entity.Entity
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling
import simx.core.worldinterface.base.WorldInterfaceActorBase
import simx.core.svaractor.semantictrait.base.CPST.cpsIterable
import simx.core.worldinterface.entity.filter.EntityFilter
import scala.language.reflectiveCalls

/**
 * Created by martin 
 * on 21/04/15.
 */
private[worldinterface] trait NewEntityRegistration extends WorldInterfaceActorBase with EntityUpdateHandling{

  private case class OnNextRegistrationTask(filter: EntityFilter, provideAnswer: Entity => Any, var sentOnce: Boolean = false, id: UUID = UUID.randomUUID())

  private var registeredEntities: collection.immutable.Set[Entity] = Set[Entity]()
  private var entityRegistrationListeners = Set[AddOnEntityAppearanceListener]()
  private var onNextRegistrationRequests  = List[OnNextRegistrationTask]()
    //List[(EntityFilter, Entity => Any)]()

  //TODO Implement if old entity registry is removed
  //addHandler[EntityRegisterRequest]{ msg => }

  //TODO Implement if old entity registry is removed
  //addHandler[EntityUnregisterRequest]{ msg => }

  addHandler[AddOnEntityAppearanceListener]{ msg =>
    bindAndCheckFilter(msg.filter)
    entityRegistrationListeners += msg
//    scala.util.continuations.reset {
//      registeredEntities.cps.filter(msg.filter).foreach(e => msg.actor ! CreationMessageNew(e, msg.reference))
//    }
  }

  addHandler[RequestEntities]{ msg =>
    bindAndCheckFilter(msg.filter)
    val _provideAnswer = provideAnswer[Set[Entity]]
    scala.util.continuations.reset {
      _provideAnswer(registeredEntities.cps.filter(msg.filter))
    }
    DelayedAnswer
  }

  addHandler[OnNextEntityAppearance]{ msg =>
    bindAndCheckFilter(msg.filter)
    onNextRegistrationRequests ::= OnNextRegistrationTask(msg.filter, provideAnswer[Entity])
    DelayedAnswer
  }

  addHandler[OnOneEntityAppearance]{ msg =>
//    println("Received request from " + sender())
    bindAndCheckFilter(msg.filter)
    val _provideAnswer = provideAnswer[Entity]
    scala.util.continuations.reset {
      val task = OnNextRegistrationTask(msg.filter, _provideAnswer)
      onNextRegistrationRequests ::= task
      val matching = registeredEntities.cps.filter(msg.filter)
      if(matching.nonEmpty && !task.sentOnce) {
        task.provideAnswer(matching.head)
        task.sentOnce = false
        onNextRegistrationRequests = onNextRegistrationRequests.filterNot(_.id == task.id)
      }
    }
    DelayedAnswer
  }

  private def bindAndCheckFilter(f : EntityFilter): Unit = {
    f.bindContext(this)
  }

  protected def _registerEntity(e: Entity): Unit = {
//    println("Added '" + e.getSimpleName + "' to the new entity registry")
    registeredEntities += e
//    e.onUpdate(notifyRegistrationListeners)
//    notifyRegistrationListeners(e)
    //var firstTime = true //TODO check why observe triggers twice
    e.onUpdate(entity => {
      //if(firstTime) firstTime = false
      //else
      notifyRegistrationListeners(entity)
    })
    notifyRegistrationListeners(e)
  }

  protected def _unRegisterEntity(e: Entity): Unit = {
    e.ignore()
    registeredEntities -= e
  }

  private def notifyRegistrationListeners(registeredEntity: Entity): Unit = {
    scala.util.continuations.reset {
      val matchingListeners = entityRegistrationListeners.cps.filter(_.filter(registeredEntity))
      matchingListeners.foreach(
        listener => listener.actor ! EntityAppearance(registeredEntity, listener.reference)
      )


      //TODO add 'partition' to cps
      val matching = onNextRegistrationRequests.cps.filter(_.filter.apply(registeredEntity))
      val nonMatching = onNextRegistrationRequests.cps.filter(!_.filter.apply(registeredEntity))
      matching.foreach { task =>
        if(!task.sentOnce) {
          task.provideAnswer(registeredEntity)
          task.sentOnce = false
        }
      }
      onNextRegistrationRequests = nonMatching
    }
  }
}