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

import simx.core.svaractor._
import simx.core.entity.Entity
import simx.core.worldinterface.eventhandling._
import simx.core.component.remote.RemoteServiceSupport
import simx.core.worldinterface.eventhandling.EventDescription
import scala.collection.mutable
import simx.core.ontology.{GroundedSymbol, types, Symbols}
import simx.core.component.ComponentCreation
import simx.core.svaractor.unifiedaccess._
import simx.core.svaractor.BunchOfSimXMessagesMessages
import simx.core.worldinterface.eventhandling.EventProviderMessage
import simx.core.entity.description.SValSet
import scala.annotation.meta.param


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
protected class WorldInterfaceActor extends SVarActor with EventProvider with RemoteServiceSupport with ComponentCreation{

  //  override protected implicit val actorContext  = this
  /** the world root, this is some kind of hack for now*/
  private val worldRoot = new RecursiveHolder
  /** the map containing all registered actors */
  private var actors     = Map[Symbol, SVarActor.Ref]()
  /** map of creation observers */
  private val creationObservers = mutable.Map[List[Symbol], Set[SVarActor.Ref]]()
  /** */
  private var nextRegHandlers = Map[List[Symbol], Set[Entity => Any]]()

  private val eventProviders = mutable.Map[GroundedSymbol, Set[SVarActor.Ref]]()

  private var foreignWorldInterfaceActors = List[SVarActor.Ref]()

  case class InvalidValueTypeException(reason : String) extends Exception

  addHandler[WorldInterfaceActorInCluster] {
    msg => {
      foreignWorldInterfaceActors = foreignWorldInterfaceActors ::: msg.worldInterfaceActor :: Nil
    }
  }

  provideInitialValuesFor{
    case (toProvide, aspect, e, given) if aspect.semanticsEqual(Symbols.name) =>
      provideInitialValues(e, aspect.getCreateParams.combineWithValues(toProvide)._1)
  }

  override protected def startUp(){
    if (SVarActor.isRemotingEnabled){
      publishActor( WorldInterfaceActor.name , self )
      observeRegistrations( WorldInterfaceActor.name ){
        ref => if (ref != self) addForeignWorldInterfaceActor(ref)
      }
    }
  }

  private def addForeignWorldInterfaceActor(ref : SVarActor.Ref){
    def createMsg(receiver : SVarActor.Ref)(msg : SVarActor.Ref => ForwardableMessage, l : List[SimXMessage]) = {
      if (SVarActor isLocal receiver) msg(receiver).forward() :: l else l
    }

    val es = worldRoot.flatten.map( entityEntry => EntityRegisterRequest(entityEntry._2, entityEntry._1) ).toList
    val ps = eventProviders.foldLeft(es : List[SimXMessage]){
      (list, kv) => kv._2.foldLeft(list){ (l, p) => createMsg(p)(ProvideEventMessage(_, kv._1), l) }
    }
    val hs = eventHandlers.foldLeft(ps){
      (list, kv) => kv._2.foldLeft(list){ (l, p) => createMsg(p._1)(RegisterHandlerMessage(_, kv._1, p._2), l) }
    }
    ref ! BunchOfSimXMessagesMessages(hs)
    foreignWorldInterfaceActors = ref :: foreignWorldInterfaceActors
  }

  private def forwardToForeignActors(msg : ForwardableMessage){
    if (!msg.isForwarded && foreignWorldInterfaceActors.nonEmpty)
      foreignWorldInterfaceActors.foreach( _.tell(msg.forward(), sender()) )
  }

  override def toString: String =
    getClass.getCanonicalName

  /**
   * adds an entity to the world root
   *
   * @param desc the entitys name
   * @param e the entity
   * @param holder you don't want to change the default value
   */
  private def setEntity( desc : List[Symbol], e : Entity,
                         holder : RecursiveHolder = worldRoot, path : List[Symbol] = Nil) : Entity = desc match {
    case name :: Nil if holder.items.get(name) == Some(e) => e
    case name :: Nil  =>
      val currentPath = (name::path).reverse
      holder.items.update( name, e )
      creationObservers.filter( x => currentPath.startsWith(x._1) ).values.foreach {
        _.foreach( _ ! CreationMessage(currentPath, e) )
      }
      val (matching, nonMatching) = nextRegHandlers.partition( x => currentPath.startsWith(x._1) )
      matching.values.foreach{ _.foreach( _.apply( e ) ) }
      nextRegHandlers = nonMatching
      e
    case head :: tail => setEntity( tail, e, holder.children.getOrElseUpdate(head, new RecursiveHolder), head :: path)
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
    case Nil => throw new Exception("provided empty list, cannot retreive entity")
  }


  private def getEntitiesBelow( desc : List[Symbol], holder : RecursiveHolder = worldRoot ) : Option[Set[(Entity, List[Symbol])]] = desc match {
    case Nil => Some(holder.flatten)
    case head :: tail => holder.children.get(head) match {
      case None if tail == Nil => Some(holder.items.get(head).toSet.map((_ : Entity,  Nil)))
      case Some(x) => getEntitiesBelow(tail, x )
      case None => None
    }
  }

  /**
   * adds a state value to be observed, triggering a WorldInterfaceEvent with the given name
   *
   * @param stateValue the state value to be observed
   * @param trigger the name of the WorldInterfaceEvent to be triggered on value changes of stateValue
   */
  private def addValueChangeTrigger[T](stateValue : SVar[T], trigger : Symbol, container : List[Symbol]) {
    observe( stateValue ){ value => emitEvent(WorldInterfaceEvent(trigger, (stateValue, container, value ) ) ) }
  }

  addHandler[ListenForRegistrationsMessage]{ msg =>
    creationObservers.update(msg.path, creationObservers.getOrElse(msg.path, Set[SVarActor.Ref]()) + msg.actor)
    getEntitiesBelow(msg.path).collect{ case set => set.foreach( t => msg.actor ! CreationMessage(t._2, t._1) ) }
  }

  addHandler[OnNextRegistration]{
    msg => getEntitiesBelow(msg.path) match {
      case Some(set) if set.nonEmpty =>
        set.head._1
      case _ =>
        nextRegHandlers = nextRegHandlers.updated(msg.path, nextRegHandlers.getOrElse(msg.path, Set()) + provideAnswer[Entity])
        DelayedAnswer
    }
  }

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
    val e = new Entity()
    e.set(types.EventDescription(new EventDescription(msg.name)))
    e.set(types.Actor(msg.provider))
    registerEntity('eventProvider :: msg.name.value.toSymbol :: Nil, e)
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
    msg => actors get msg.destination match{
      case Some(dst) => dst ! msg.msg
      case None =>
    }
  }


  addHandler[ActorRegisterRequest]{ msg =>
    forwardToForeignActors(msg)
    actors += msg.name -> msg.actor
  }

  addHandler[ActorListingRequest]{
    msg => msg.replyTo ! ActorListingReply(actors.keys.toList)
  }

  addHandler[EntityCreateRequest]{ msg =>
    forwardToForeignActors(EntityRegisterRequest(msg.name, setEntity(msg.name, new Entity)))
  }

  addHandler[EntityRegisterRequest]{ msg =>
    forwardToForeignActors(msg)
    setEntity(msg.name, msg.e )
  }

  //  addHandler[StateValueCreateRequest[_]]{
  //    case msg : StateValueCreateRequest[_] => forwardToForeignActors(
  //      EntityRegisterRequest(msg.container, addStateValue(SVarImpl(msg.value), msg.desc, msg.container) )
  //    )
  //  }

  addHandler[ExternalStateValueObserveRequest[_]]{
    msg => addValueChangeTrigger(msg.ovalue, msg.trigger, msg.container)
  }

  addHandler[ActorEnumerateRequest]{
    msg => Some(actors.keys.toList)
  }

  addHandler[ComponentLookupRequest]{
    msg => delayedReplyWith(handleComponentMap)(_.flatMap(_._2).get(msg.name))
  }

  addHandler[ComponentLookUpByType]{
    msg => delayedReplyWith(handleComponentMap)(_.get(msg.componentType.value.toSymbol).map(_.toList).getOrElse(Nil))
  }

  addHandler[AllComponentsLookupRequest]{
    msg => delayedReplyWith(handleComponentMap)(_.flatMap(_._2))
  }

  private def handleComponentMap( handler : Map[Symbol, Map[Symbol, SVarActor.Ref]] => Any) = {
    getEntitiesBelow(List(Symbols.component.value.toSymbol)) match {
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

  addHandler[EntityLookupRequest]{
    msg => getEntity(msg.name)
  }

  addHandler[ActorLookupRequest]{
    msg => actors.get(msg.name)
  }

  addHandler[EntityUnregisterRequest]{ msg =>
    forwardToForeignActors(msg)
    worldRoot.remove(msg.e)
  }

  addHandler[InternalStateValueObserveRequest[_]]{
    msg => getEntity(msg.nameE).collect{
      case entity => entity.get(msg.c).forall{ _.values.foreach{ case svar : SVar[_] => addValueChangeTrigger(svar, msg.trigger, msg.nameE) } }
    }
  }

  addHandler[EntityGroupLookupRequest]{
    msg => getEntitiesBelow(msg.name).collect{ case set => set.map(_._1) }.getOrElse(Set())
  }


  private val knownRelations = SValSet()

//  private val otherKnownRelations = Map[Relation, Entity]()

  addHandler[AddRelation]{ msg =>
    val relation = msg.r
    knownRelations.update(relation)
  }

  addHandler[RemoveRelation]{ msg =>
    knownRelations.remove(msg.r)
  }

  addHandler[HandleRelationRequest[_, _]]{ msg =>
    if (msg.r.isLeft) {
      knownRelations.getOrElse(msg.r.description.sVarIdentifier, Nil).map(_ as msg.r.description.asConvertibleTrait).
        filter(_.obj equals msg.r.getKnownValue).
        map(x => MapKey(types.Entity, AnnotationSet()) -> types.Entity(x.subj)).toMap
    } else {
        knownRelations.getOrElse(msg.r.description.sVarIdentifier, Nil).map(_ as msg.r.description.asConvertibleTrait).
          filter(_.subj equals msg.r.getKnownValue).
          map(x => MapKey(types.Entity, AnnotationSet()) -> types.Entity(x.obj)).toMap
    }
  }
}


protected class RecursiveHolder{
  val items = mutable.Map[Symbol, Entity]()
  val children = mutable.Map[Symbol, RecursiveHolder]()

  def getItemsRecursively : Set[Entity] =
    children.foldLeft(items.values.toSet)( _ ++ _._2.getItemsRecursively )

  def flatten : Set[(Entity, List[Symbol])] = {
    val flatChilds = children.flatMap{ tuple => tuple._2.flatten.map( t => t._1 -> (tuple._1 :: t._2 ) ) }
    flatChilds.toSet ++ items.map(tuple => tuple._2 -> List(tuple._1))
  }

  def remove(e : Entity) {
    items.retain( (_, b) => b != e )
    children.values.foreach( _.remove(e) )
  }
}