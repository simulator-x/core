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

import simx.core.svaractor.{SVarActor, SVar}
import simx.core.entity.Entity
import simx.core.entity.typeconversion.ConvertibleTrait
import eventhandling.{EventHandler, EventDescription, EventProvider, Event}
import simx.core.component.Component
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag


/**
 * Created by IntelliJ IDEA.
 * User: stephan_rehfeld
 * Date: 27.11.11
 * Time: 21:14
 * To change this template use File | Settings | File Templates.
 */

/**
 * The World Interface. This object shall be used to provide to state value known to the entire programm, as well as
 * for actors and components. (In fact this is only a wrapper around the WorldInterfaceActor, which is doing all the
 * work)
 *
 * For now, one has to ensure that the id's of registered components/entities/state values/actors are unique, later
 * this shall be done automatically
 *
 *
 * @author dwiebusch
 * @author Stephan Rehfeld
 * date: 02.07.2010
 */
trait WorldInterfaceHandling extends SVarActor {

  /**
   * registers an actor
   *
   * @param name the name under which the actor will be accessible after being registered
   * @param actor the actor to be registered
   */
  final protected def registerActor(name: Symbol, actor: SVarActor.Ref) {
    WorldInterfaceActor ! ActorRegisterRequest(name, actor)
  }


  final protected def registerComponent( c : Component) {
    WorldInterfaceActor ! ComponentRegisterRequest(c.componentName, c.self)
  }
  /**
   * registers an entity
   *
   * @param name the name under which the entity will be accessible after being registered
   * @param e the entity to be registered
   */
  final protected def registerEntity(name : Symbol, e : Entity) {
    WorldInterfaceActor ! EntityRegisterRequest(name :: Nil, e)
  }

  final protected  def registerEntity(name : List[Symbol], e : Entity) {
    WorldInterfaceActor ! EntityRegisterRequest(name, e)
  }

  final protected def unregisterEntity(e : Entity) {
    WorldInterfaceActor ! EntityUnregisterRequest(e)
  }


  /**
   * registers a given EventHandler for all events having the same name as the given event
   *
   * @param handler the handler to be registered
   * @param event an event the handler is registered for
   */
  final protected def requireEvent( handler : EventHandler, event : EventDescription ) {
    WorldInterfaceActor ! RegisterHandlerMessage(handler, event)
  }

  /**
   * registers an EventProvider and stores the event it will provide. Furthermore tells all
   * EventHandlers that have requiered the event (more precisely: an event with the same name)
   * of the existence of the Provider
   *
   * @param provider the EventProvider to be registered
   * @param event the event that the provider provides
   */
  final protected def provideEvent(provider : EventProvider,  event : EventDescription ) {
    WorldInterfaceActor ! ProvideEventMessage(provider, event)
  }

  /**
   * removes an EventHandler for one specific or all events, after what it won't be notified about
   * new Provider providing the event / all events the handler was registered for
   *
   * @param handler the EventHandler to be unregistered
   * @param event the event the handler will be unregistered from (optional)
   */
  final protected def removeEventHandler( handler : EventHandler, event : Option[EventDescription] = None ) {
    WorldInterfaceActor ! UnRegisterHandlerMessage( handler, event )
  }

  /**
   * removes an EventProvider from the list of registered providers, therefore new handlers won't
   * be notified about this specific provider either for the given event or all events (if no event is given)
   *
   * @param provider the provider to be unregistered
   * @param event the event which will no more be provided (optional)
   */
  final protected def removeEventProvider( provider : EventProvider, event : Option[Event] = None ) {
    WorldInterfaceActor ! UnRegisterProviderMessage( provider )
  }

  /**
   *  creates a new state value
   *
   * @param desc information on the svar to be created
   * @param entityName name of the entity which shall contain the new state value
   */
  final protected def createStateValue[T](desc : ConvertibleTrait[T], value : T, entityName : Symbol) {
    WorldInterfaceActor ! StateValueCreateRequest(desc, value, entityName :: Nil)
  }

  final protected def createStateValue[T](desc : ConvertibleTrait[T], value : T, entityName : List[Symbol]) {
    WorldInterfaceActor ! StateValueCreateRequest(desc, value, entityName)
  }

  /**
   * adds a new WorldInterfaceEvent to be triggered, when the given (external) state value changes
   *
   * @param stateValue the state value to be observed
   * @param container the name of the entity containing the svar
   * @param trigger the WorldInterfaceEvents name being triggered on the state value change
   */
  final protected def addValueChangeTrigger[T](stateValue : SVar[T], container : Symbol, trigger : Symbol) {
    WorldInterfaceActor ! ExternalStateValueObserveRequest(stateValue, container :: Nil, trigger)
  }

  final protected def addValueChangeTrigger[T](stateValue : SVar[T], container : List[Symbol], trigger : Symbol) {
    WorldInterfaceActor ! ExternalStateValueObserveRequest(stateValue, container, trigger)
  }

  /**
   * adds a new WorldInterfaceEvent to be triggered, when the given (internal) state value changes
   *
   * @param c information on the state value to be observed (has to be known by the WorldInterfaceActor)
   * @param entityName the name of the entity containing the specified state value
   * @param trigger the WorldInterfaceEvents name being triggered on the state value change
   */
  final protected def addValueChangeTrigger[T]( c : ConvertibleTrait[T], entityName : Symbol, trigger : Symbol) {
    WorldInterfaceActor ! InternalStateValueObserveRequest(c, entityName :: Nil, trigger)
  }

  final protected def addValueChangeTrigger[T]( c : ConvertibleTrait[T], entityName : List[Symbol], trigger : Symbol) {
    WorldInterfaceActor ! InternalStateValueObserveRequest(c, entityName, trigger)
  }

  final protected def handleActorList(handler : List[Symbol] => Unit) {
    nonBlockingHandlerNew[List[Symbol]]( ActorEnumerateRequest(), handler )
  }

  final protected def handleActor( name : Symbol )( handler : Option[SVarActor.Ref] => Unit ) {
    nonBlockingHandlerNew[Option[SVarActor.Ref]](ActorLookupRequest(name), handler)
  }

  final protected def handleStateValue[T : TypeTag]( c : ConvertibleTrait[T], container : Symbol )( handler : Option[SVar[T]] => Unit ) {
    nonBlockingHandlerNew(StateValueLookupRequest(c, List(container)), handler)
  }

  final protected def handleStateValue[T : TypeTag]( c : ConvertibleTrait[T], container : List[Symbol] )( handler : Option[SVar[T]] => Unit ) {
    nonBlockingHandlerNew(StateValueLookupRequest(c, container), handler)
  }

  final protected def handleEntity( name : Symbol )( handler : Option[Entity] => Any ) {
    nonBlockingHandlerNew[Option[Entity]]( EntityLookupRequest( name :: Nil ), handler )
  }

  final protected def handleComponent(name : Symbol)( handler : Option[SVarActor.Ref] => Any ) {
    nonBlockingHandlerNew( ComponentLookupRequest(name ), handler )
  }

  // Evil functions
  final protected def handleSVarValue[T : TypeTag]( c : ConvertibleTrait[T], entity : Symbol )( handler : Option[T] => Unit ) {
    nonBlockingHandlerNew( SVarReadRequest(c, entity :: Nil), handler)
  }

  final protected def handleSVarValue[T : TypeTag](c : ConvertibleTrait[T], entity : List[Symbol])( handler : Option[T] => Unit ){
    nonBlockingHandlerNew( SVarReadRequest(c, entity), handler)
  }

  final protected def readSVar[T : ClassTag : TypeTag]( svar : SVar[T])( handler : T => Unit ) {
    nonBlockingHandlerNew(ReadRequest(svar), handler)
  }

  final protected def handleRegisteredEntities( path : List[Symbol] )( handler : Set[Entity] => Any ) {
    nonBlockingHandlerNew[Set[Entity]](EntityGroupLookupRequest(path), handler)
  }

  final protected def handleRegisteredComponents(handler : Map[Symbol, SVarActor.Ref] => Unit) {
    nonBlockingHandlerNew[Map[Symbol, SVarActor.Ref]](AllComponentsLookupRequest(), handler)
  }

  final protected def registerForCreationOf( path : List[Symbol] )(implicit actorContext : SVarActor.Ref) {
    WorldInterfaceActor ! ListenForRegistrationsMessage( actorContext, path )
  }

  final protected def onNextCreation( path : List[Symbol] )( f : Entity => Any ) {
    nonBlockingHandlerNew[Entity](OnNextRegistration(path), f)
  }

  /**
   *
   * this one is a bit advanced, so here is what happens:
   * the function takes two parameters, one function that instanciates the message to be send and the handler which
   * shall be executed when an answer to that message is sent.
   * Initially an ID is generated and stored to identify the message which is sent. Then a single-use-handler is
   * installed, which uses that generated id. The installed handler simply calls the function "handler" which is
   * provided as a parameter, applying the value returned by the worldInterfaceActor.
   * Finally a message is sent to the worldInterfaceActor, which contains an handler (executed by the
   * worldInterfaceActor), that sends the id, value tuple back, causing the invocation of the installed handler
   *
   */
  protected def nonBlockingHandlerNew[T](msg : Any, handler : T => Any)
                                        (implicit actorContext : SVarActor, m : ClassTag[T], t : TypeTag[T]) {
    actorContext.ask(WorldInterfaceActor.self, msg){
      answer : T => handler(answer)
      //case _ => println("Error: wrong answer type")
    }
  }
}