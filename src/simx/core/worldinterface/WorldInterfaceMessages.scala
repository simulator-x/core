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
import simx.core.entity.description.{SValBase, SValSet}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.ontology
import simx.core.ontology._
import simx.core.svaractor.semantictrait.base.{Base, Thing}
import simx.core.svaractor.unifiedaccess.{Relation, Request}
import simx.core.svaractor.{SVar, SVarActor, SimXMessage}
import simx.core.worldinterface.eventhandling.{Event, EventDescription}

/* author: dwiebusch
* date: 10.09.2010
*/


/**
 * Base Class for events handled by the WorldInterfaceActor
 *
 * @author dwiebusch
 */
case class WorldInterfaceEvent( specificName : Symbol, value : Any)
  extends Event( Symbols.event, new SValSet())

/**
 * Message sent to the WorldInterface Actor, to register a new handler for the given event
 *
 * @author dwiebusch
 * @param handler the handler to be registered
 */
private case class RegisterHandlerMessage( handler : SVarActor.Ref, name : GroundedSymbol, restriction : Option[PartialFunction[Event, Boolean]] )(implicit val sentBy : SVarActor.Ref)
  extends SimXMessage with ForwardableMessage{ protected def copy = RegisterHandlerMessage(handler, name, restriction)(sentBy) }


private case class ProvideEventMessage(provider : SVarActor.Ref, name : GroundedSymbol, event : Option[Event] = None )(implicit val sentBy : SVarActor.Ref)
  extends SimXMessage with ForwardableMessage{ protected def copy = ProvideEventMessage(provider, name, event)(sentBy) }

/**
 * Message sent to the WorldInterface Actor, to unregister a handler
 *
 * @author dwiebusch
 * @param handler the handler to be unregistered
 */
private case class UnRegisterHandlerMessage( handler : SVarActor.Ref, e : Option[EventDescription] = None )(implicit val sentBy : SVarActor.Ref)
  extends SimXMessage with ForwardableMessage{ protected def copy: UnRegisterHandlerMessage = UnRegisterHandlerMessage(handler, e)(sentBy) }

private case class UnRegisterProviderMessage( provider : SVarActor.Ref, e : Option[EventDescription] = None )(implicit val sentBy : SVarActor.Ref)
  extends SimXMessage with ForwardableMessage { protected def copy = UnRegisterProviderMessage(provider, e)(sentBy) }

/**
 * Message sent to the WorldInterface Actor, to create a new state value
 *
 * @author dwiebusch
 * @param value the initial value of the created state value
 * @param container the id of the entity, the state value is injected into
 */
private case class StateValueCreateRequest[T](desc: ConvertibleTrait[T], value: T, container: List[Symbol])


/**
 * Message sent to the WorldInterface Actor, to register a handler for a value change of the gieven state value
 *
 * @author dwiebusch
 * @param ovalue the state value to be observed
 * @param trigger the name of the WorldInterfaceEvent which is triggered on value change of ovalue
 */
private case class ExternalStateValueObserveRequest[T](ovalue: SVar[T], container : List[Symbol], trigger: Symbol)

//Commented due to non-usage by martin, 20-04-2015
///**
// * Message sent to the WorldInterface Actor, to observe an state value, already registered with the WorldInterfaceActor
// *
// * @author dwiebusch
// * @param nameE the name of the entity containing the state value
// * @param trigger the name of the WorldInterfaceEvent which is triggered on value change of the state value
// */
//private case class InternalStateValueObserveRequest[T](c: ConvertibleTrait[T], nameE : List[Symbol], trigger: Symbol)

/**
 * Message sent to the WorldInterface Actor, to create a new entity
 *
 * @author dwiebusch
 * @param name the name of the entity to be created
 */
//Commented due to non-usage by martin, 20-04-2015
//private case class EntityCreateRequest(name: List[Symbol])

/**
 * Message sent to the WorldInterface Actor, to register an existing actor
 *
 * @author dwiebusch
 * @param name the name under which the actor will be accessible after being registered
 * @param actor the actor to be registered
 */
private case class ActorRegisterRequest(name : Symbol, actor : SVarActor.Ref)(implicit val sentBy : SVarActor.Ref)
  extends SimXMessage with ForwardableMessage{ protected def copy = ActorRegisterRequest(name, actor)(sentBy) }

/**
 * Message sent to the WorldInterface Actor, to receive a listing of all known actors
 *
 * @author dwiebusch
 * @param replyTo the actor to whom the list will be sent
 */
private case class ActorListingRequest(replyTo : SVarActor.Ref)

/**
 * Message sent by the WorldInterface Actor, in reply to an ActorListingRequest
 *
 * @author dwiebusch
 * @param list the list containing the names of all actors known to the WorldInterfaceActor
 */
private case class ActorListingReply(list : List[Symbol])

/**
 * Message sent to the WorldInterface Actor, to forward a message to another actor
 *
 * @author dwiebusch
 * @param destination the symbol under which the other actor was registered
 * @param msg the message to be forwarded
 */
private case class ForwardMessageRequest(destination : Symbol, msg : Any)

/**
 * Message sent to the WorldInterface Actor, to register an existing component
 *
 * @author dwiebusch
 * @param name the name under which the component will be accessible after being registered
 * @param cType the type of component to be registered
 * @param ref the actor reference of the component
 *
 */
private case class ComponentRegisterRequest(name : Symbol, cType : ontology.GroundedSymbol, ref : SVarActor.Ref) (implicit val actorContext : SVarActor.Ref)
  extends SimXMessage with ForwardableMessage{ protected def copy = ComponentRegisterRequest(name, cType, ref)(actorContext)}


/**
 * Message sent to the WorldInterface Actor, receive a list of all actors known to the WorldInterfaceActor
 * FOR INTERNAL USAGE ONLY
 *
 * @author dwiebusch
 */
//private case class ActorEnumerateRequest(future : SyncVar[Option[List[Symbol]]])
private case class ActorEnumerateRequest()

/**
 * Message sent to the WorldInterface Actor, to look up a specific actor
 * FOR INTERNAL USAGE ONLY
 *
 * @author dwiebusch
 * @param name the id of the actor to be looked up
 */
//private case class ActorLookupRequest(name : Symbol, future : SyncVar[Option[Actor]])
private case class ActorLookupRequest(name : Symbol)

/**
  * Message sent to the WorldInterface Actor, to look up a specific component
 * FOR INTERNAL USAGE ONLY
 *
 * @author dwiebusch
 * @param name the name under which the component was registered
 */
//private case class ComponentLookupRequest(name : Symbol, future : SyncVar[Option[Component]])
private case class ComponentLookupRequest(name : Symbol)
//private case class AllComponentsLookupRequest(future : SyncVar[Map[Symbol, Component]])
private case class AllComponentsLookupRequest()
//
private case class ComponentLookUpByType(componentType : ontology.GroundedSymbol)

private case class AddRelation(r :  SValBase[Relation, _ <: Relation])
private case class RemoveRelation(r : SValBase[Relation, _ <: Relation])
private case class HandleRelationRequest[S <: Entity, O <: Entity](r : Request[S, O])

private[core] case class RegisterSVarDescription[T, B, X <: Base, S <: Thing](desc : SValDescription[T, B, X, S])
private case class ObserveSVarDescRegistrations(sender : SVarActor.Ref)
private case class GetRegisteredSVarDescriptions()

private[worldinterface] trait ForwardableMessage extends SimXMessage{
  protected var forwarded = false
  protected def copy : ForwardableMessage
  def isForwarded : Boolean = forwarded
  def forward() : ForwardableMessage = {
    val retVal = copy
    retVal.forwarded = true
    retVal
  }
}