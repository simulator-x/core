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
import simx.core.component.Component
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.worldinterface.eventhandling.{EventDescription, EventProvider, EventHandler, Event}
import simx.core.entity.description.SValSet
import simx.core.ontology.Symbols
import simx.core.svaractor.{SVarActor, SVar}
import java.lang.Exception
import java.util.UUID

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
case class RegisterHandlerMessage( handler : EventHandler, event : EventDescription )


case class ProvideEventMessage(provider : EventProvider, event : EventDescription )

protected class HandlerMSG[T]( handler : T => Any ) {
  private case class Reply[U](id : UUID, value : U)
  private val sender : Option[SVarActor.Ref] = None
  private val id = UUID.randomUUID()

  def reply( value : T ) {
    sender.getOrElse( throw new Exception( "No Handler was registered" ) ) ! Reply(id, value)
  }
}

case class TestHandlerMSG(handler : Int => Unit) extends HandlerMSG(handler)

/**
 * Message sent to the WorldInterface Actor, to unregister a handler
 *
 * @author dwiebusch
 * @param handler the handler to be unregistered
 */
case class UnRegisterHandlerMessage( handler : EventHandler, e : Option[EventDescription] = None )

case class UnRegisterProviderMessage( provider : EventProvider, e : Option[EventDescription] = None )

/**
 * Message sent to the WorldInterface Actor, to create a new state value
 *
 * @author dwiebusch
 * @param value the initial value of the created state value
 * @param container the id of the entity, the state value is injected into
 */
case class StateValueCreateRequest[T](desc: ConvertibleTrait[T], value: T, container: List[Symbol])


/**
 * Message sent to the WorldInterface Actor, to write to a registered state value
 *
 * @author dwiebusch
 * @param container the id of the entity which contains the state value
 * @param newValue the value to be set
 */
case class StateValueSetRequest[T](c : ConvertibleTrait[T], container : List[Symbol], newValue : T)

/**
 * Message sent to the WorldInterface Actor, to register a handler for a value change of the gieven state value
 *
 * @author dwiebusch
 * @param ovalue the state value to be observed
 * @param trigger the name of the WorldInterfaceEvent which is triggered on value change of ovalue
 */
case class ExternalStateValueObserveRequest[T](ovalue: SVar[T], container : List[Symbol], trigger: Symbol)

/**
 * Message sent to the WorldInterface Actor, to observe an state value, already registered with the WorldInterfaceActor
 *
 * @author dwiebusch
 * @param nameE the name of the entity containing the state value
 * @param trigger the name of the WorldInterfaceEvent which is triggered on value change of the state value
 */
case class InternalStateValueObserveRequest[T](c: ConvertibleTrait[T], nameE : List[Symbol], trigger: Symbol)

/**
 * Message sent to the WorldInterface Actor, to create a new entity
 *
 * @author dwiebusch
 * @param name the name of the entity to be created
 */
case class EntityCreateRequest(name: List[Symbol])

/**
 * Message sent to the WorldInterface Actor, to register an existing entity
 *
 * @author dwiebusch
 * @param name the name under which the entity will be accessible after being registered
 * @param e the entity to be registered
 */
case class EntityRegisterRequest(name : List[Symbol], e : Entity)
case class EntityUnregisterRequest(e : Entity)

/**
 * Message sent to the WorldInterface Actor, to register an existing actor
 *
 * @author dwiebusch
 * @param name the name under which the actor will be accessible after being registered
 * @param actor the actor to be registered
 */
case class ActorRegisterRequest(name : Symbol, actor : SVarActor.Ref)

/**
 * Message sent to the WorldInterface Actor, to receive a listing of all known actors
 *
 * @author dwiebusch
 * @param replyTo the actor to whom the list will be sent
 */
case class ActorListingRequest(replyTo : SVarActor.Ref)

/**
 * Message sent by the WorldInterface Actor, in reply to an ActorListingRequest
 *
 * @author dwiebusch
 * @param list the list containing the names of all actors known to the WorldInterfaceActor
 */
case class ActorListingReply(list : List[Symbol])

/**
 * Message sent to the WorldInterface Actor, to forward a message to another actor
 *
 * @author dwiebusch
 * @param destination the symbol under which the other actor was registered
 * @param msg the message to be forwarded
 */
case class ForwardMessageRequest(destination : Symbol, msg : Any)

/**
 * Message sent to the WorldInterface Actor, to register an existing component
 *
 * @author dwiebusch
 * @param name the name under which the component will be accessible after being registered
 * @param component the component to be registered
 */
case class ComponentRegisterRequest( name : Symbol, component : SVarActor.Ref )


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
 * Message sent to the WorldInterface Actor,  to look up a specific entity
 * FOR INTERNAL USAGE ONLY
 *
 * @author dwiebusch
 * @param name the id of the entity to be looked up
 */
//private case class EntityLookupRequest(name : List[Symbol], future : SyncVar[Option[Entity]])
private case class EntityLookupRequest(name : List[Symbol])
//private case class EntityGroupLookupRequest(name : List[Symbol], future : SyncVar[Option[Set[Entity]]])
private case class EntityGroupLookupRequest(name : List[Symbol])

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

/**
 * Message sent to the WorldInterface Actor, to look up a specific state value
 * FOR INTERNAL USAGE ONLY
 *
 * @author dwiebusch
 * @param container the id of the entity containing the state value to be looked up
 */
//private case class StateValueLookupRequest[T]( c : ConvertibleTrait[T], container : List[Symbol], future : SyncVar[Option[SVar[T]]])
private case class StateValueLookupRequest[T]( c : ConvertibleTrait[T], container : List[Symbol])

//private case class SVarReadRequest[T]( c : ConvertibleTrait[T], entity : List[Symbol], future : SyncVar[Option[T]] )
private case class SVarReadRequest[T]( c : ConvertibleTrait[T], entity : List[Symbol] )

//private case class ReadRequest[T]( svar : SVar[T], future : SyncVar[T] )
private case class ReadRequest[T]( svar : SVar[T])

private case class SVarWriteRequest[T](svar : SVar[T], value : T)

private case class ListenForRegistrationsMessage( actor : SVarActor.Ref, path : List[Symbol] )

private case class OnNextRegistration( path : List[Symbol])

case class CreationMessage( path : List[Symbol], e : Entity )

