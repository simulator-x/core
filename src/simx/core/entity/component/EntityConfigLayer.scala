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

package simx.core.entity.component

import scala.collection.mutable
import simx.core.svaractor.handlersupport.HandlerSupport
import simx.core.svaractor.{SVarActor, SimXMessage}
import simx.core.entity.description._
import simx.core.entity.typeconversion._
import simx.core.entity.Entity
import java.util.UUID
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling
import scala.annotation.meta.param

/* author: dwiebusch
* date: 02.09.2010
*/

//! request for dependencies
protected[entity] case class GetDependenciesMsg( id : java.util.UUID,  asp : EntityAspect )
                                               (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage

//! answer to request for dependencies
protected[entity] case class GetDependenciesAns( requestId : java.util.UUID,
                                                 deps : Set[Dependencies],
                                                 asp : EntityAspect )
                                               (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage

//!
case class GetInitialConfigValuesMsg(id : java.util.UUID, asp : EntityAspect, e : Entity )
                                    (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage

//!
case class FinalizeComponentConfigMsg(e : Entity )(implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage

//! request for initial values
// ToDO: Document
case class GetInitialValuesMsg( id : java.util.UUID,
                                p : Set[ConvertibleTrait[_]],
                                asp : EntityAspect,
                                e : Entity, given : SValSet )
                              (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage
//! answer to request for initial values
// TODO: Document
case class GetInitialValuesAns( id : java.util.UUID, initialValues : SValSet )
                              (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage

//! information of completed svar insertion
case class SVarsCreatedMsg( id : java.util.UUID, entity : Entity )
//! request to create svars
case class CreateSVarsMsg( id : java.util.UUID, toCreate : List[ProvideConversionInfo[_,_]],
                           entity : Entity,
                           queue : List[(SVarActor.Ref, List[ProvideConversionInfo[_,_]])] )
                         (implicit @(transient @param) actor : SVarActor.Ref)
  extends SimXMessage

//! information about a new entity
// TODO: Document
case class EntityCompleteMsg( asp : EntityAspect, e : Entity )
                            (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage


/**
 *  the trait to be mixed in by all components that participate in the entity creation process
 */
trait EntityConfigLayer extends SVarActor with HandlerSupport with EntityUpdateHandling{
  //! the map of unanswered create requests
  private val openCreateRequests = mutable.Map[Entity, (SVarActor.Ref, Set[ConvertibleTrait[_]])]()
  //! the set of known removable entities
  private val knownRemovableEntities = mutable.Set[Entity]()
  //! the reverse mapping from entities to ids
  private val entityToIDMap = mutable.Map[Entity, java.util.UUID]()

  /**
   *  returns a set of additional convertibletraits specifying the addidional svars provided by this component
   * @return the set
   */
  protected def getAdditionalProvidings( aspect : EntityAspect ) : Set[ConvertibleTrait[_]] = Set()

  /**
   *  returns the set of dependencies that need to be met for this component to provide particular initial values
   * @param aspect the aspect the be realized
   * @return the set
   */
  protected def getDependencies( aspect : EntityAspect ) : Set[Dependencies] =
    aspect.getDependencies

  /**
   *  method to be implemented by each component. Will be called when an entity has to be removed from the
   * internal representation.
   * @param e the Entity to be removed
   */
  protected def removeFromLocalRep(e : Entity)

  //add handler to answer get dependency messages
  addHandler[GetDependenciesMsg]{ msg =>
    var deps    = getDependencies(msg.asp)
    val toAdd   = getAdditionalProvidings(msg.asp).filter{ p => deps.find(_.providings.objects.contains(p)).isEmpty }
    val allReqs = deps.flatMap( a => a.requirings.objects ).toSeq

    if (toAdd.nonEmpty) deps = deps + Dependencies(Providing(toAdd.toSeq : _*), Requiring(allReqs : _*))
    if (deps.isEmpty) deps = Set(Dependencies(Requiring()))
    msg.sender ! GetDependenciesAns( msg.id, deps, msg.asp )(self)
  }


  //add handler to answer get initial value messages
  addHandler[GetInitialValuesMsg]{ msg =>
    addNewRequest(msg.id, msg.e, msg.sender, msg.p)
    requestInitialValues( msg.p, msg.asp, msg.e, msg.given )
  }

  protected def addNewRequest(id : UUID, e : Entity, sender : SVarActor.Ref, toProvide : Set[ConvertibleTrait[_]]){
    openCreateRequests.update( e, sender -> toProvide )
    entityToIDMap += e -> id
  }

  //add handler to react to CreateSVarsMsgs
  addHandler[CreateSVarsMsg]{ msg =>
    def handler(entity : Entity) {
      val newQueue = injectSVarCreation(entity) ::: msg.queue
      knownRemovableEntities += entity
      newQueue.headOption.collect{
        case (actor, Nil) => actor ! SVarsCreatedMsg( msg.id, entity )
        case (actor, set) => actor ! CreateSVarsMsg(msg.id, set, entity, newQueue.tail)
      }
    }

    msg.toCreate.foldRight(handler(_)){ (toAdd, hnd) => toAdd.injectSVar(_)(hnd) }.apply(msg.entity)
  }

  protected def injectSVarCreation(e : Entity) : List[(SVarActor.Ref, List[ProvideConversionInfo[_, _]])] =
    Nil

  //add handler react to EntityCompleteMsgs
  addHandler[EntityCompleteMsg]{
    msg => entityConfigComplete(msg.e, msg.asp)
  }

  /**
   * initiates removal of an entity. Therefore it has to be known by this instance
   * @param e the entity to be removed
   */
  protected def removeEntity( e : Entity ) {
    knownRemovableEntities -= e
  }

  /**
   * provideInitialValues has to be called within this method with the full set of initial values to be provided
   * @note the component should create its local representation within this method
   * @param toProvide the convertibletraits for which values shall be provided
   * @param aspect the aspect providing the context for this method call
   * @param e the entity to be filled
   * @param given a set of create parameters that were already provided
   *
   */
  protected def requestInitialValues( toProvide : Set[ConvertibleTrait[_]], aspect : EntityAspect,
                                      e : Entity, given : SValSet )

  /**
   * used to integrate the entity into the simulation
   * @param e the entity to be integrated
   * @param aspect the aspect which the component has to process
   */
  protected def entityConfigComplete( e : Entity , aspect : EntityAspect )

  /**
   * to be called to inform the entity creation actor of the new initial values
   * @param e the entity for which initial values shall be provided
   * @param providings the list of provided values
   */
  protected def provideInitialValues( e : Entity, providings : SValSet ) {
    val (creator, toProvide) = openCreateRequests.getOrElse(e, throw new Exception("Unknown entity: " + e))
    val open = toProvide.filterNot( providings contains _ )
    if (open.nonEmpty) throw new Exception("Error: " + open.mkString(" and ") + " was/were not provided")
    creator ! GetInitialValuesAns(entityToIDMap(e), providings)(self)
    openCreateRequests.remove(e)
  }
}
