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

package simx.core.worldinterface

import simx.core.entity.Entity
import simx.core.entity.description.SValSet
import simx.core.ontology._
import simx.core.svaractor._
import simx.core.svaractor.semantictrait.base.{Base, Thing}
import simx.core.svaractor.unifiedaccess._
import simx.core.worldinterface.base.WorldInterfaceActorBase
import simx.core.worldinterface.entity._
import simx.core.worldinterface.eventhandling.{EventDescription, EventProviderMessage, _}

import scala.annotation.meta.param
import scala.collection.mutable

/* author: dwiebusch
* date: 10.09.2010
*/

/**
 * @author Stephan Rehfeld
 */
case class WorldInterfaceActorInCluster( worldInterfaceActor : SVarActor.Ref )
                                       (implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage

object WorldInterfaceActor extends SingletonActor(new WorldInterfaceActor, "worldInterface" )

/**
 * The World Interface Actor, which is doing all the Interfacing work
 *
 * @author dwiebusch
 *
 */
protected class WorldInterfaceActor extends WorldInterfaceActorBase
  with EventProvider with EntityRegistration with NewEntityRegistration
{
  //  override protected implicit val actorContext  = this

  /** the map containing all registered actors */
  private var actors     = Map[Symbol, SVarActor.Ref]()

  private val eventProviders = mutable.Map[GroundedSymbol, Set[SVarActor.Ref]]()

  case class InvalidValueTypeException(reason : String) extends Exception


  //TODO Remove/refactor if old entity registration is removed
  protected def _onEntityUnRegistration(e : Entity): Unit = {
    _unRegisterEntity(e)
  }

  //TODO Remove/refactor if old entity registration is removed
  protected def _onEntityRegistration(e: Entity): Unit = {
    _registerEntity(e)
  }

  def generateCopyOfRegistry(): List[SimXMessage] = {
    //Helper function to make intellij more better ;)
    def toEntityRegisterRequest(entityEntry: (Entity, List[Symbol])): SimXMessage =
      EntityRegisterRequest(entityEntry._2, entityEntry._1).asInstanceOf[SimXMessage]

    //TODO Use private field 'registeredEntities'
    val es = worldRoot.flatten.map(toEntityRegisterRequest).toList
    val ps = eventProviders.foldLeft(es : List[SimXMessage]){
      (list, kv) => kv._2.foldLeft(list){ (l, p) => createMsg(p)(ProvideEventMessage(_, kv._1), l) }
    }
    eventHandlers.foldLeft(ps){
      (list, kv) => kv._2.foldLeft(list){ (l, p) => createMsg(p._1)(RegisterHandlerMessage(_, kv._1, p._2), l) }
    }
  }

  override def toString: String =
    getClass.getCanonicalName

  //Commented due to non-usage by martin, 20-04-2015
//  /**
//   * adds a state value to be observed, triggering a WorldInterfaceEvent with the given name
//   *
//   * @param stateValue the state value to be observed
//   * @param trigger the name of the WorldInterfaceEvent to be triggered on value changes of stateValue
//   */
//  private def addValueChangeTrigger[T](stateValue : SVar[T], trigger : Symbol, container : List[Symbol]) {
//    observe( stateValue ){ value => emitEvent(WorldInterfaceEvent(trigger, (stateValue, container, value ) ) ) }
//  }

  addHandler[RegisterHandlerMessage]{ msg =>
    internalRequireEvent( msg.handler, msg.name, msg.restriction )
    //Send notification about providers to handler
    eventProviders.get(msg.name).collect{ case providers => msg.handler ! EventProviderMessage(providers, msg.name)}
    forwardToForeignActors(msg)
  }

  addHandler[UnRegisterHandlerMessage]{ msg =>
    internalRemoveEventHandler( msg.handler, msg.e )
    forwardToForeignActors(msg)
  }

  addHandler[ProvideEventMessage]{ msg =>
    forwardToForeignActors(msg)
    eventProviders.update( msg.name, eventProviders.getOrElse(msg.name, Set[SVarActor.Ref]()) + msg.provider )
    val e = new Entity(new EntityDescription(name = "Event[" + msg.name.toString + "]").desc)
    e.set(types.EventDescription(new EventDescription(msg.name)))
    e.set(types.Actor(msg.provider))
    addToWorldRoot('eventProvider :: msg.name.toSymbol :: Nil, e)
    eventHandlers.get(msg.name) collect {
      case set => set.foreach{ _._1 ! EventProviderMessage(Set(msg.provider), msg.name, msg.event) }
    }
  }

  addHandler[UnRegisterProviderMessage]{ msg =>
    forwardToForeignActors(msg)
    msg.e match {
      case Some(event) =>
        eventProviders.update(event.name, eventProviders.getOrElse(event.name, Set[SVarActor.Ref]()).filterNot( _ == msg.provider ))
      case None => for ( (event, providers) <- eventProviders)
        eventProviders.update(event, providers.filterNot( _ == msg.provider))
    }
  }

  addHandler[ForwardMessageRequest]{
    msg => Match(actors get msg.destination){
      case Some(dst) => dst ! msg.msg
      case None => ()
    }
  }

  protected var svarDescRegistry = Map[String, SValDescription[_, _,_ <: Base,_ <: Thing]]()
  protected var svarDescObservers = Set[SVarActor.Ref]()

  addHandler[RegisterSVarDescription[_, _,_ <: Base,_ <: Thing]]{ msg =>
    svarDescRegistry += msg.desc.ontoLink.getOrElse(msg.desc.sVarIdentifier.name) -> msg.desc
    svarDescObservers.foreach( _ ! msg)
  }

  addHandler[GetRegisteredSVarDescriptions]{
    msg => svarDescRegistry
  }

  addHandler[ObserveSVarDescRegistrations]{ msg =>
    svarDescRegistry.foreach( tuple => msg.sender ! RegisterSVarDescription(tuple._2))
    svarDescObservers += msg.sender
  }

  addHandler[ActorRegisterRequest]{ msg =>
    forwardToForeignActors(msg)
    actors += msg.name -> msg.actor
  }

  addHandler[ActorListingRequest]{
    msg => msg.replyTo ! ActorListingReply(actors.keys.toList)
  }

  //Commented due to non-usage by martin, 20-04-2015
//  addHandler[EntityCreateRequest]{ msg =>
//    forwardToForeignActors(EntityRegisterRequest(msg.name, addToWorldRoot(msg.name, new Entity)))
//  }

  //  addHandler[StateValueCreateRequest[_]]{
  //    case msg : StateValueCreateRequest[_] => forwardToForeignActors(
  //      EntityRegisterRequest(msg.container, addStateValue(SVarImpl(msg.value), msg.desc, msg.container) )
  //    )
  //  }

  //Commented due to non-usage by martin, 20-04-2015
//  addHandler[ExternalStateValueObserveRequest[_]]{
//    msg => addValueChangeTrigger(msg.ovalue, msg.trigger, msg.container)
//  }

  addHandler[ActorEnumerateRequest]{
    msg => Some(actors.keys.toList)
  }

  addHandler[ActorLookupRequest]{
    msg => actors.get(msg.name)
  }

  //Commented due to non-usage by martin, 20-04-2015
//  addHandler[InternalStateValueObserveRequest[_]]{
//    msg => getEntity(msg.nameE).collect{
//      case entity => entity.get(msg.c).forall{ _.values.foreach{ case svar : SVar[_] => addValueChangeTrigger(svar, msg.trigger, msg.nameE) } }
//    }
//  }


  private val knownRelations = SValSet()

//  private val otherKnownRelations = Map[Relation, Entity]()

  addHandler[AddRelation]{ msg =>
    knownRelations.update(msg.r.asSVal)
  }

  addHandler[RemoveRelation]{ msg =>
    knownRelations.remove(msg.r.asSVal)
  }

  addHandler[HandleRelationRequest[_, _]]{ msg =>
    if (msg.r.isLeft) {
      knownRelations.getOrElse(msg.r.description.sVarIdentifier, Nil).map(_ as msg.r.description.asConvertibleTrait).
        filter(_.getObject equals msg.r.getKnownValue).
        map(x => MapKey(types.Entity, AnnotationSet()) -> x.getSubject).toMap
    } else {
        knownRelations.getOrElse(msg.r.description.sVarIdentifier, Nil).map(_ as msg.r.description.asConvertibleTrait).
          filter(_.getSubject equals msg.r.getKnownValue).
          map(x => MapKey(types.Entity, AnnotationSet()) -> x.getObject).toMap
    }
  }
}


