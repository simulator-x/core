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

import scala.collection.mutable
import simx.core.entity.Entity
import simx.core.component.SingletonActor
import simx.core.svaractor._
import handlersupport.Types
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.worldinterface.eventhandling._
import simx.core.ontology.GroundedSymbol


/* author: dwiebusch
* date: 10.09.2010
*/

/**
 * @author Stephan Rehfeld
 */
case class WorldInterfaceActorInCluster( worldInterfaceActor : SVarActor.Ref )
                                       (implicit @transient actorContext : SVarActor.Ref) extends SimXMessage

object WorldInterfaceActor extends SingletonActor(new WorldInterfaceActor, "worldInterface" ){
  def set[T](svar : SVar[T], value : T) {
    self ! SVarWriteRequest(svar, value)
  }
}

/**
 * The World Interface Actor, which is doing all the Interfacing work
 *
 * @author dwiebusch
 *
 */
protected class WorldInterfaceActor extends SVarActor with EventProvider {
  /** the world root, this is some kind of hack for now*/
  private val worldRoot = new RecursiveHolder
  /** an internal list to store handlers for every registered event (and the internal handling stuff) */
  //private var handlers   = List[Types.handler_t]()
  /** an internal map, containing all triggered world interface events (or at least their names) */
  //private var triggers   = Map[SVar[_], (Symbol, Symbol)]()
  /** the map containing all registered actors */
  private var actors     = Map[Symbol, SVarActor.Ref]()
  /** the map containing all registered components */
  private var components = Map[Symbol, SVarActor.Ref]()
  /** map of creation observers */
  private val creationObservers = mutable.Map[List[Symbol], Set[SVarActor.Ref]]()
  private var nextRegHandlers = Map[List[Symbol], Set[Entity => Any]]()

  private val eventProviders = mutable.Map[GroundedSymbol, Set[EventProvider]]()

  private var foreignWorldInterfaceActors = List[SVarActor.Ref]()

  case class InvalidValueTypeException(reason : String) extends Exception

  addHandler[WorldInterfaceActorInCluster] {
    msg => {
      foreignWorldInterfaceActors = foreignWorldInterfaceActors ::: msg.worldInterfaceActor :: Nil
    }
  }

  override def toString: String =
    getClass.getCanonicalName

  /**
   * adds an entity to the world root
   *
   * @param desc the entitys name
   * @param e an entity (if None, a new one will be created)
   * @param holder you don't want to change the default value
   */
  private def setEntity( desc : List[Symbol], e : Option[Entity] = None,
                         holder : RecursiveHolder = worldRoot, path : List[Symbol] = Nil) {
    desc match{
      case name :: Nil  =>
        val (newEntity, currentPath) = (e.getOrElse(new Entity), (name::path).reverse)
        holder.items.update( name, newEntity )
        creationObservers.filter( x => currentPath.startsWith(x._1) ).values.foreach {
          _.foreach( _ ! CreationMessage(currentPath, holder.items.get(name).get) )
        }
        nextRegHandlers.filter( x => currentPath.startsWith(x._1)).values.foreach{
          _.foreach( _.apply( newEntity ) )
        }
        nextRegHandlers = nextRegHandlers.filterNot( x => currentPath.startsWith(x._1) )
      case head :: tail => setEntity( tail, e, holder.children.getOrElseUpdate(head, new RecursiveHolder), head :: path)
      case Nil => throw new Exception("provided empty list, cannot insert entity")
    }
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
    case Nil => throw new Exception("provided empty list, cannot retreive entity")
  }


  private def getEntitiesBelow( desc : List[Symbol], holder : RecursiveHolder = worldRoot ) : Option[Set[Entity]] = desc match {
    case Nil => Some(holder.getItemsRecursively)
    case head :: tail => holder.children.get(head) match {
      case None if tail == Nil => Some(holder.items.get(head).toSet)
      case Some(x) => getEntitiesBelow(tail, x )
      case None => None
    }
  }

  override protected def internalRequireEvent(handler: EventHandler, event: EventDescription ) {
    super.internalRequireEvent(handler, event)
    eventProviders.get(event.name) collect {
      case set => handler.self ! EventProviderMessage(set, event)
    }
  }

  /**
   * updates an entity by injecting the given state value
   *
   * @param stateValue the state value to be injected
   * (a new one is created if no one is known under the given name)
   */
  private def addStateValue[T](stateValue : SVar[T], desc : ConvertibleTrait[T], containerName : List[Symbol]) {
    setEntity(containerName, Some(getEntity(containerName).getOrElse(new Entity).injectSVar(stateValue, desc)) )
  }

  /**
   * adds a state value to be observed, triggering a WorldInterfaceEvent with the given name
   *
   * @param stateValue the state value to be observed
   * @param trigger the name of the WorldInterfaceEvent to be triggered on value changes of stateValue
   */
  private def addValueChangeTrigger[T](stateValue : SVar[T], trigger : Symbol, container : List[Symbol]) {
    observe( stateValue){ (value : T) => emitEvent(WorldInterfaceEvent(trigger, (stateValue, container, value ) ) ) }
  }



  /**
   * handles a message. if this is called from the WorldInterfaceActor's thread, the handling is executed instantaneously
   * otherwise the message is forwarded to the WorldInterfaceActor
   */
  private[worldinterface] def handleMessage(msg : Any)(implicit context : SVarActor.Ref) {
    if (context == self) applyHandlers(msg) else self.!(msg)(sender)
  }

  /**
   * checks the known handlers if they can handle the event and handles a given event if a matching handler was found
   * otherwise calls the catchAll handler
   *
   * @param handlers a list of handlers to be checked
   * @param catchAll the handler to be returned if no handler contained in the list matches
   * @return a handler that can handle the given event or catchAll if no such handler was found in the given list
   */
  private def handleEvent(handlers : List[Types.handler_t], catchAll : Types.handler_t): Types.handler_t =
    handlers match {
      case head :: tail => head orElse handleEvent(tail, catchAll)
      case Nil => catchAll
    }

  addHandler[ListenForRegistrationsMessage]{ msg =>
    creationObservers.update(msg.path, creationObservers.getOrElse(msg.path, Set[SVarActor.Ref]()) + msg.actor)
    getEntitiesBelow(msg.path).collect{ case set => set.foreach( msg.actor ! CreationMessage(msg.path, _) ) }
  }

  addHandler[OnNextRegistration]{
    msg => getEntitiesBelow(msg.path) match {
      case Some(set) if set.nonEmpty =>
        set.head
      case _ =>
        nextRegHandlers = nextRegHandlers.updated(msg.path, nextRegHandlers.getOrElse(msg.path, Set()) + provideAnswer[Entity])
        DelayedAnswer
    }
  }

  addHandler[RegisterHandlerMessage]{
    msg => internalRequireEvent( msg.handler, msg.event )
  }

  addHandler[UnRegisterHandlerMessage]{
    msg => internalRemoveEventHandler( msg.handler, msg.e )
  }

  addHandler[ProvideEventMessage]{ msg =>
    eventProviders.update( msg.event.name, eventProviders.getOrElse(msg.event.name, Set[EventProvider]()) + msg.provider )
    eventHandlers.get(msg.event.name) collect {
      case set => set.foreach{ p => p._1.self ! EventProviderMessage(Set(msg.provider), msg.event)}
    }
  }

  addHandler[UnRegisterProviderMessage]{ msg =>
    msg.e match {
      case Some(event) =>
        eventProviders.update(event.name, eventProviders.getOrElse(event.name, Set[EventProvider]()).filterNot( _ == msg.provider ))
      case None => for ( (event, providers) <- eventProviders)
        eventProviders.update(event, providers.filterNot( _ == msg.provider))
    }
  }

  addHandler[ForwardMessageRequest]{
    msg => actors get msg.destination match{
      case Some(dst) => dst ! msg.msg
      case None =>
    }
  }


  private def writeHelper[T](msg : SVarWriteRequest[T]) {
    msg.svar.set(msg.value)
  }

  addHandler[SVarWriteRequest[_]]( msg => writeHelper(msg) )

  addHandler[ActorRegisterRequest]{
    msg =>  actors += msg.name -> msg.actor
  }

  addHandler[ActorListingRequest]{
    msg => msg.replyTo ! ActorListingReply(actors.keys.toList)
  }

  addHandler[ComponentRegisterRequest]{
    msg => components += msg.name -> msg.component
  }

  addHandler[EntityCreateRequest]{
    msg => setEntity(msg.name)
  }

  addHandler[EntityRegisterRequest]{
    msg => setEntity(msg.name, Some(msg.e) )
  }

  addHandler[StateValueCreateRequest[_]]{
    case msg: StateValueCreateRequest[_] => addStateValue(SVarImpl(msg.value), msg.desc, msg.container)
  }

  addHandler[ExternalStateValueObserveRequest[_]]{
    msg => addValueChangeTrigger(msg.ovalue, msg.trigger, msg.container)
  }

  addHandler[ActorEnumerateRequest]{
    msg => Some(actors.keys.toList)
  }

  addHandler[ComponentLookupRequest]{
    msg => components.get(msg.name)
  }

  addHandler[AllComponentsLookupRequest]{
    msg => components
  }

  addHandler[EntityLookupRequest]{
    msg => getEntity(msg.name)
  }

  addHandler[ActorLookupRequest]{
    msg => actors.get(msg.name)
  }

  addHandler[EntityUnregisterRequest]{
    msg => worldRoot.remove(msg.e)
  }

  addHandler[InternalStateValueObserveRequest[_]]{
    msg => getEntity(msg.nameE).collect{
      case entity => entity.get(msg.c).collect{ case svar =>addValueChangeTrigger(svar, msg.trigger, msg.nameE) }
    }
  }

  addHandler[StateValueSetRequest[_]]{
    case msg: StateValueSetRequest[_] => getEntity(msg.container).collect{
      case entity => set( entity.get(msg.c).head, msg.newValue )
    }
  }

  addHandler[EntityGroupLookupRequest]{
    msg => getEntitiesBelow(msg.name).getOrElse(Set())
  }

  addHandler[StateValueLookupRequest[_]]{
    case StateValueLookupRequest(c, container) => getEntity(container) match {
      case Some(entity) => entity.get(c).headOption
      case None => None
    }
  }


  addHandler[ReadRequest[_]]( msg => readTypeHelper(msg) )

  private def readTypeHelper[T]( in : ReadRequest[T] ){
    delayedReplyWith[T](in.svar.get(_)(actorContext))( x => x )
  }

  private def handlerBind[T]( sVar : SVar[T]) {
    delayedReplyWith[Option[T]]( x => sVar.get(y => x.apply(Some(y)))(actorContext))( x => x )
  }

  addHandler[SVarReadRequest[_]]{
    case SVarReadRequest(c, entity) => getEntity(entity) match {
      case Some(e) => e.get(c).headOption match {
        case Some(svar) => handlerBind( svar )
        case None => None
      }
      case None => None
    }
  }
}


class RecursiveHolder{
  val items = mutable.Map[Symbol, Entity]()
  val children = mutable.Map[Symbol, RecursiveHolder]()

  def getItemsRecursively : Set[Entity] =
    children.foldLeft(items.values.toSet)( (a, b) => a ++ b._2.getItemsRecursively )

  def remove(e : Entity) {
    items.retain( (a,b) => b != e )
    children.values.foreach( _.remove(e) )
  }
}