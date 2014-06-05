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

package simx.core.ontology.entities

import simx.core.entity.description._
import simx.core.helper.NewExecScheme
import simx.core.ontology.Symbols.{entityCreation, entityDescription}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.svaractor.SVarActor
import simx.core.component.Component
import simx.core.entity.component.EntityCreationHandling
import simx.core.entity.Entity

/**
 * @author dwiebusch
 * Date: 06.11.11
 * Time: 19:57
 */


object EntityCreationComponent{
  protected[EntityCreationComponent] var started = false
  def isStarted = started
}

class EntityCreationComponent extends SVarActor with Component with EntityCreationHandling {
  EntityCreationComponent.started = true

  def componentName = 'EntityCreationComponent_Core
  def componentType = entityCreation

  protected def requestInitialValues(tp: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity, given: SValSet) {
    if (aspect.semanticsEqual(entityDescription))
      aspect.getCreateParams.getAllValuesFor(simx.core.ontology.types.EntityDescription) match {
        case head :: tail => create(e, head, tail).exec{ set => provideInitialValues( e, SValSet(set.toSeq :_*) ) }
        case Nil => provideInitialValues(e, SValSet())
      }
    else
      println("error: unknown aspect for entity creation component: " + aspect)
  }

  private def create( parent : Entity,
                      head : GeneralEntityDescription[_ <: Entity],
                      tail : List[GeneralEntityDescription[_ <: Entity]] ) = {
    tail.foldLeft(NewExecScheme(sValFrom(parent, head))){ _ and sValFrom(parent, _) }
  }

  private def sValFrom[T <: Entity]( parent : Entity,  desc : GeneralEntityDescription[T] ) =
    ( handler : SVal[_] => Any ) => realizeAsSVal(desc, parent, handler)

  private def realizeAsSVal[T <: Entity]( desc : GeneralEntityDescription[T], parent : Entity, handler : SVal[T] => Any ){
    val newAspects = desc.aspects
    newAspects.foreach(_.setParent(parent))
    val newDescription = desc.copy( newAspects )
    val newHandler     =  { (a : T) => handler(SVal(desc.typeDef)(a)) }
    realize(newDescription)( newHandler)
  }

  protected def entityConfigComplete(e: Entity, aspect: EntityAspect){}
  protected def removeFromLocalRep(e: Entity){}

  protected def performSimulationStep() {
    this.simulationCompleted()
  }

  protected def configure(params: SValSet){}
}