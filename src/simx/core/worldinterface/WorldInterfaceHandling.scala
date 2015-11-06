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

import simx.core.component.{DisableAspect, EnableAspect}
import simx.core.entity.Entity
import simx.core.entity.description.{EntityAspect, SValBase}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.ontology.{GroundedSymbol, SValDescription}
import simx.core.svaractor.SVarActor
import simx.core.svaractor.semantictrait.base.{Base, Thing}
import simx.core.svaractor.unifiedaccess._
import simx.core.worldinterface.base.WorldInterfaceHandlingBase
import simx.core.worldinterface.entity.{NewEntityRegistrationHandling, EntityRegistrationHandling, NewEntityRegistrationHandling$}
import simx.core.worldinterface.eventhandling.{Event, EventDescription}

/**
 * The World Interface. This object shall be used to provide to state value known to the entire programm, as well as
 * for actors and components. (In fact this is only a wrapper around the WorldInterfaceActor, which is doing all the
 * work)
 *
 * For now, one has to ensure that the id's of registered components/entities/state values/actors are unique, later
 * this shall be done automatically
 *
 * @author Stephan Rehfeld
 * @author Dennis Wiebusch
 * @author Martin Fischbach
 * date: 02.07.2010
 */
trait WorldInterfaceHandling extends WorldInterfaceHandlingBase
  with EntityRegistrationHandling with NewEntityRegistrationHandling {

  final def setRelation(relSVal : SValBase[Relation, _ <: Relation])(implicit context : EntityUpdateHandling){
    WorldInterfaceActor ! AddRelation(relSVal)
  }

  final def removeRelation(relSVal : SValBase[Relation, _ <: Relation])(implicit context : EntityUpdateHandling){
    WorldInterfaceActor ! RemoveRelation(relSVal)
  }

  def enableAspect[T <: Entity](e : T, asp : EntityAspect, handler : Boolean => Unit = e => {}){
    handleComponents(asp.componentType){
      list => (if (asp.targets.nonEmpty) list.filter( tuple => asp.targets.contains(tuple._1)  ) else list).foreach{
        tuple => ask[Boolean](tuple._2, EnableAspect(e, asp))( handler )
      }
    }
  }

  def disableAspect[T <: Entity](e : T, asp : EntityAspect, handler : Boolean => Unit = e => {}){
    handleComponents(asp.componentType){
      list => (if (asp.targets.nonEmpty) list.filter( tuple => asp.targets.contains(tuple._1)  ) else list).foreach{
        tuple => ask[Boolean](tuple._2, DisableAspect(e, asp))( handler )
      }
    }
  }

  /**
   * registers an actor
   *
   * @param name the name under which the actor will be accessible after being registered
   * @param actor the actor to be registered
   */
  final protected def registerActor(name: Symbol, actor: SVarActor.Ref) {
    WorldInterfaceActor ! ActorRegisterRequest(name, actor)
  }

  /**
   * registers a given EventHandler for all events having the same name as the given event
   *
   * @param handler the handler to be registered
   * @param event an event the handler is registered for
   */
  final protected def requireEvent( handler : SVarActor.Ref, event : EventDescription ) {
    WorldInterfaceActor ! RegisterHandlerMessage(handler, event.name, event.restriction)
  }

  /**
   * removes an EventHandler for one specific or all events, after what it won't be notified about
   * new Provider providing the event / all events the handler was registered for
   *
   * @param handler the EventHandler to be unregistered
   * @param event the event the handler will be unregistered from (optional)
   */
  final protected def removeEventHandler( handler : SVarActor.Ref, event : Option[EventDescription] = None ) {
    WorldInterfaceActor ! UnRegisterHandlerMessage( handler, event )
  }

  /**
   * removes an EventProvider from the list of registered providers, therefore new handlers won't
   * be notified about this specific provider either for the given event or all events (if no event is given)
   *
   * @param provider the provider to be unregistered
   * @param event the event which will no more be provided (optional)
   */
  final protected def removeEventProvider( provider : SVarActor.Ref, event : Option[Event] = None ) {
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

  //Commented due to non-usage by martin, 20-04-2015
//  /**
//   * adds a new WorldInterfaceEvent to be triggered, when the given (external) state value changes
//   *
//   * @param stateValue the state value to be observed
//   * @param container the name of the entity containing the svar
//   * @param trigger the WorldInterfaceEvents name being triggered on the state value change
//   */
//  final protected def addValueChangeTrigger[T](stateValue : SVar[T], container : Symbol, trigger : Symbol) {
//    WorldInterfaceActor ! ExternalStateValueObserveRequest(stateValue, container :: Nil, trigger)
//  }
//
//  final protected def addValueChangeTrigger[T](stateValue : SVar[T], container : List[Symbol], trigger : Symbol) {
//    WorldInterfaceActor ! ExternalStateValueObserveRequest(stateValue, container, trigger)
//  }

  //Commented due to non-usage by martin, 20-04-2015
//  /**
//   * adds a new WorldInterfaceEvent to be triggered, when the given (internal) state value changes
//   *
//   * @param c information on the state value to be observed (has to be known by the WorldInterfaceActor)
//   * @param entityName the name of the entity containing the specified state value
//   * @param trigger the WorldInterfaceEvents name being triggered on the state value change
//   */
//  final protected def addValueChangeTrigger[T]( c : ConvertibleTrait[T], entityName : Symbol, trigger : Symbol) {
//    WorldInterfaceActor ! InternalStateValueObserveRequest(c, entityName :: Nil, trigger)
//  }
//
//  final protected def addValueChangeTrigger[T]( c : ConvertibleTrait[T], entityName : List[Symbol], trigger : Symbol) {
//    WorldInterfaceActor ! InternalStateValueObserveRequest(c, entityName, trigger)
//  }

  final protected def handleActorList(handler : List[Symbol] => Unit) {
    nonBlockingHandler[List[Symbol]]( ActorEnumerateRequest(), handler )
  }

  final protected def handleActor( name : Symbol )( handler : Option[SVarActor.Ref] => Unit ) {
    nonBlockingHandler[Option[SVarActor.Ref]](ActorLookupRequest(name), handler)
  }

  final protected def handleComponent(name : Symbol)( handler : Option[SVarActor.Ref] => Any ) {
    nonBlockingHandler( ComponentLookupRequest(name ), handler )
  }

  final protected def handleComponents(componentType : GroundedSymbol)( handler : List[(Symbol, SVarActor.Ref)] => Any ){
    nonBlockingHandler[List[(Symbol, SVarActor.Ref)]]( ComponentLookUpByType( componentType ), handler )
  }

  final protected def handleRegisteredComponents(handler : Map[Symbol, SVarActor.Ref] => Unit) {
    nonBlockingHandler[Map[Symbol, SVarActor.Ref]](AllComponentsLookupRequest(), handler)
  }

  final protected def handleRegisteredSVarDescriptions(handler : Map[String, SValDescription[_, _,_ <: Base,_ <: Thing]] => Unit) =
    nonBlockingHandler[Map[String, SValDescription[_, _,_ <: Base,_ <: Thing]]](GetRegisteredSVarDescriptions(), handler)

  final protected def onSVarDescRegistration(handler : SValDescription[_, _,_ <: Base,_ <: Thing] => Unit){
    addHandler[RegisterSVarDescription[_, _,_ <: Base,_ <: Thing]](msg => handler(msg.desc))
    WorldInterfaceActor ! ObserveSVarDescRegistrations(self)
  }
}