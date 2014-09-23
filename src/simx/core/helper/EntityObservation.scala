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

package simx.core.helper

import simx.core.entity.Entity
import simx.core.ontology
import simx.core.svaractor.StateParticle
import simx.core.svaractor.unifiedaccess.{EntityUpdateHandling, StateParticleInfo}

/**
 * Created by dwiebusch on 22.08.14
 */
trait EntityObservation extends EntityUpdateHandling{

  protected sealed abstract class Change[T](val annotations : Set[ontology.Annotation], value : Option[StateParticle[T]])
  protected case class Add[T](e : Entity, info : StateParticleInfo[T]) extends Change[T](info.annotations, Some(info.svar))
  protected case class Remove[T](e : Entity, info : StateParticleInfo[T]) extends Change[T](info.annotations, Some(info.svar))


  protected def onEntityUpdate(e : Entity)(handler : Change[_] => Unit) = {
    var knownStateParticles = e.getAllStateParticles
    knownStateParticles.foreach(added => handler(Add(e, added)))
    e.onUpdate{ newE =>
      val newStateParticles = newE.getAllStateParticles
      val addedStateParticles = newStateParticles.filterNot( p => knownStateParticles.exists( _.svar equals p.svar ) )
      val removedStateParticles = knownStateParticles.filterNot( p => newStateParticles.exists( _.svar equals p.svar ) )
      knownStateParticles = knownStateParticles.filterNot( p => removedStateParticles.exists(_.svar equals p.svar)) ++ addedStateParticles
      addedStateParticles.foreach(added => handler(Add(newE, added)))
      removedStateParticles.foreach(removed => handler(Remove(newE, removed)))
    }
  }
}
