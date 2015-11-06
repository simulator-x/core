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

import simx.core.component.ComponentCreation
import simx.core.entity.Entity
import simx.core.ontology.{types, Symbols}
import simx.core.svaractor.SVarActor
import simx.core.worldinterface.{AllComponentsLookupRequest, ComponentLookUpByType, ComponentLookupRequest}
import simx.core.worldinterface.base.WorldInterfaceActorBase

/**
 * Created by martin 
 * on 21/04/15.
 */
private[worldinterface] trait EntityRegistration extends WorldInterfaceActorBase with ComponentCreation {

  /** the world root, this is some kind of hack for now*/
  protected val worldRoot = new RecursiveHolder

  private var entityRegistrationListeners = Map[List[Symbol], Set[SVarActor.Ref]]()
  private var onNextRegistrationRequests  = Map[List[Symbol], Set[Entity => Any]]()

  addHandler[EntityRegisterRequest]{ msg =>
    forwardToForeignActors(msg)
    addToWorldRoot(msg.name, msg.e)
    _onEntityRegistration(msg.e)
  }

  addHandler[EntityUnregisterRequest]{ msg =>
    forwardToForeignActors(msg)
    removeEntityFromRegistry(msg.e)
  } 
  
  addHandler[AddEntityRegistrationListener]{ msg =>
    entityRegistrationListeners = entityRegistrationListeners.updated(msg.path, entityRegistrationListeners.getOrElse(msg.path, Set[SVarActor.Ref]()) + msg.actor)
    getEntitiesBelow(msg.path).collect{ case set => set.foreach( t => msg.actor ! CreationMessage(t._2, t._1) ) }
  }

  addHandler[OnOneRegistration]{
    msg => Match( getEntitiesBelow(msg.path)){
      case Some(set) if set.nonEmpty => set.head._1
      case _ =>
        onNextRegistrationRequests = onNextRegistrationRequests.updated(msg.path, onNextRegistrationRequests.getOrElse(msg.path, Set()) + provideAnswer[Entity])
        DelayedAnswer
    }
  }

  addHandler[OnNextRegistration]{ msg =>
    onNextRegistrationRequests = onNextRegistrationRequests.updated(msg.path, onNextRegistrationRequests.getOrElse(msg.path, Set()) + provideAnswer[Entity])
    DelayedAnswer
  }

  addHandler[LookupEntity]{
    msg => getEntity(msg.name)
  }
  
  addHandler[LookupEntities]{ msg =>
    Match(getEntitiesBelow(msg.name)){
      case Some(set) => set.map(_._1)
      case None => Set()
    }
  }
  
  addHandler[ComponentLookupRequest]{
    msg => delayedReplyWith(handleComponentMap)(_.flatMap(_._2).get(msg.name))
  }

  addHandler[ComponentLookUpByType]{
    msg => delayedReplyWith(handleComponentMap)(_.get(msg.componentType.toSymbol).map(_.toList).getOrElse(Nil))
  }

  addHandler[AllComponentsLookupRequest]{
    msg => delayedReplyWith(handleComponentMap)(_.flatMap(_._2))
  }

  protected def _onEntityUnRegistration(e : Entity)
  protected def _onEntityRegistration(e: Entity)

  provideInitialValuesFor{
    case (toProvide, aspect, e, given) if aspect.semanticsEqual(Symbols.name) =>
      provideInitialValues(e, aspect.getCreateParams.combineWithValues(toProvide)._1)
  }

  /**
   * method to be implemented by each component. Will be called when an entity has to be removed from the
   * internal representation.
   * @param e the Entity to be removed
   */
  override protected def removeFromLocalRep(e: Entity) {
    removeEntityFromRegistry(e)
  }

  def removeEntityFromRegistry(e: Entity): Unit = {
    worldRoot.remove(e)
    _onEntityUnRegistration(e)
  }

  /**
   * adds an entity to the world root
   *
   * @param desc the entities name
   * @param e the entity
   * @param holder you don't want to change the default value
   */
  //TODO: Remove and realize the behavior with the new approach if necessary
  protected[worldinterface] def addToWorldRoot( desc : List[Symbol], e : Entity,
                              holder : RecursiveHolder = worldRoot, path : List[Symbol] = Nil) : Entity = desc match {
    case name :: Nil if holder.items.get(name) == Some(e) => e
    case name :: Nil  =>
      val currentPath = (name::path).reverse
      holder.items.update( name, e )
      entityRegistrationListeners.filter( x => currentPath.startsWith(x._1) ).values.foreach {
        _.foreach( _ ! CreationMessage(currentPath, e) )
      }
      val (matching, nonMatching) = onNextRegistrationRequests.partition( x => currentPath.startsWith(x._1) )
      matching.values.foreach{ _.foreach( _.apply( e ) ) }
      onNextRegistrationRequests = nonMatching
      e
    case head :: tail => addToWorldRoot( tail, e, holder.children.getOrElseUpdate(head, new RecursiveHolder), head :: path)
    case Nil => throw new Exception("provided empty list, cannot insert entity")
  }

  /**
   * retrieves an entity from the world root
   *
   * @param desc the name of the entity to be retrieved
   * @param holder you don't want to change the default value
   * @return the entity to be retrieved, or none if no entity was registered under the given name
   */

  private def getEntity( desc : List[Symbol], holder : RecursiveHolder = worldRoot ) : Option[Entity] = desc match {
    case name :: Nil  => holder.items.get(name)
    case head :: tail => getEntity(tail, holder.children.apply(head))
    case Nil => throw new Exception("provided empty list, cannot retrieve entity")
  }

  private def getEntitiesBelow( desc : List[Symbol], holder : RecursiveHolder = worldRoot ) : Option[Set[(Entity, List[Symbol])]] = desc match {
    case Nil => Some(holder.flatten)
    case head :: tail => holder.children.get(head) match {
      case None if tail == Nil =>
        Some(holder.items.get(head).toSet.map((_ : Entity,  Nil))) //.map((_ : Entity,  desc))???
      case Some(x) => getEntitiesBelow(tail, x )
      case None => None
    }
  }

  private def handleComponentMap( handler : Map[Symbol, Map[Symbol, SVarActor.Ref]] => Any) : Unit = {
    getEntitiesBelow(List(Symbols.component.Symbol.toSymbol)) match {
      case Some(set) if set.size > 0 =>
        var counter = 0
        var resultMap : Map[Symbol, Map[Symbol, SVarActor.Ref]] = Map()
        set.map{ tuple =>
          (h : (List[Symbol], SVarActor.Ref) => Any) => tuple._1.get(types.Component).foreach{ a => h(tuple._2, a) }
        }.foreach{
          _.apply{ (path, actor) =>
            resultMap = resultMap.updated(path.head, resultMap.getOrElse(path.head, Map()).updated(path.tail.head, actor))
            counter += 1
            if (counter == set.size)
              handler(resultMap)
          }
        }
      case _ =>
        handler(Map())
    }
  }
}
