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

package simx.core.component

import scala.collection.mutable
import simx.core.entity.component.EntityConfigLayer
import simx.core.entity.description.{SVal, SValSet}
import simx.core.svaractor.{SimXMessage, SVarActor}
import simx.core.worldinterface.{ComponentRegisterRequest, WorldInterfaceActor}
import simx.core.svaractor.synclayer.{SyncGroupHandling, SyncGroup, SyncGroupLeader}
import simx.core.ontology.GroundedSymbol

/* author: dwiebusch
 * date: 27.08.2010
 */

/**
 *
 *  Used to create ConfigureComponentMessages
 */
object ConfigureComponent {
  /**
   *  Creates a ConfigureComponentMessage using Actor.self as sender
   */
  def apply(createParams: SValSet)(implicit actorContext : SVarActor.Ref) = new ConfigureComponentMessage( createParams )
  /**
   *  Creates a ConfigureComponentMessage using Actor.self as sender and params to create a new SValSet
   */
  def apply(params: SVal[_]*)(implicit actorContext : SVarActor.Ref) = new ConfigureComponentMessage( new SValSet(params:_*))
}

/**
 *
 *  This message is sent to a component to configure it initially
 *
 * @param createParams A set of CVars that this component needs to be initialized
 *                     Obligatory and optional CVars for individual Components should be defined in their respective documentations.
 */
class ConfigureComponentMessage( val createParams: SValSet )
                               (implicit @transient actorContext : SVarActor.Ref) extends SimXMessage
/**
 *
 * This message is sent by a component when the configuration process is finished.
 *
 * It will be sended to the sender of the configuration messages.
 *
 */
case class ComponentConfigured( )(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage

/**
 * This message triggers a component to perform the next simulation step.
 *
 * @author Stephan Rehfeld
 */
case class PerformSimulationStep()(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage

/**
 * This message sets the frequency of a component.
 *
 * @author Stephan Rehfeld
 *
 * @param frequency The frequency of the component.
 */
case class SetFrequency( frequency : Frequency )(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage


/**
 * This message is sent to an observer of the stepcount after a component finished the simulation step.
 *
 * @author Stephan Rehfeld
 *
 * @param stepCount The step count.
 */
case class CurrentStepCount( stepCount : Long )(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage

/**
 * The sender of this message gets notified about the current step count.
 *
 * @author Stephan Rehfeld
 *
 * @param target the target
 *
 */
case class ObserveStepCount( target : Option[SVarActor.Ref] = None )(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage

/**
 * The sender of this message will not be notified about changes of the step count an more.
 *
 * @author Stephan Rehfeld
 *
 * @param target the target
 *
 */
case class IgnoreStepCount( target : Option[SVarActor.Ref] = None  )(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage

/**
 * This message sets the trigger targets of this component. All actors in the list trigger will receive
 * a [[simx.core.component.PerformSimulationStep]] message on the begin or end of of the current simulation step.
 * If the boolean value in the tuple is true, the trigger is sent at the begin of the frame. If the boolean value is
 * false the trigger is sent q@at the end.
 *
 * @author Stephan Rehfeld
 *
 * @param trigger A list of all trigger targets and a flag when they are triggers. True means on step begin, false means
 *                on step end.
 */
case class SetTrigger( trigger : List[(SVarActor.Ref,Boolean)] )(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage {

  require( trigger != null, "The parameter 'trigger' must not be 'null'!" )

}

/**
 * This message indicates a component to synchronize on a sync group and to be triggered by it. A new available world
 * state results in a trigger of the component.
 *
 * @author Stephan Rehfeld
 *
 * @param syncGroup The sync group to synchronize on.
 */
case class TriggerOnGroup( syncGroup : SyncGroup )(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage {
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
}


/**
 *  trait to provide SVarActors with EntityConfigLayer
 *  @author Dennis Wiebusch
 *  @author Stephan Rehfeld
 */
trait Component extends SVarActor with EntityConfigLayer with SyncGroupLeader with SyncGroupHandling {

  var frequency : Frequency = Passive()

  var triggerOnStepBegin = List[SVarActor.Ref]()
  var triggerOnStepEnd = List[SVarActor.Ref]()

  var stepCount : Long = 0
  var stepCountObserver = Set[SVarActor.Ref]()

  /**
   *  (re)configure the component
   * @param params the configuration params
   */
  protected def configure(params : SValSet)

  //add handler for configuration msgs
  addHandler[ConfigureComponentMessage]{ msg =>
    configure(msg.createParams)
    msg.sender ! ComponentConfigured()
  }

  addHandler[SetFrequency]  {
    msg =>
      this.frequency = msg.frequency
  }

  addHandler[SetTrigger]  {
    msg =>
      triggerOnStepBegin = List[SVarActor.Ref]()
      triggerOnStepEnd = List[SVarActor.Ref]()

      for( t <- msg.trigger ) {
        if( t._2 )
          triggerOnStepBegin = triggerOnStepBegin ::: t._1 :: Nil
        else
          triggerOnStepEnd = triggerOnStepEnd ::: t._1 :: Nil
      }
  }

  addHandler[ObserveStepCount] {
    msg =>
      this.stepCountObserver = this.stepCountObserver + (if( msg.target.isDefined ) msg.target.get else msg.sender)

  }

  addHandler[IgnoreStepCount] {
    msg =>
      this.stepCountObserver = this.stepCountObserver - (if( msg.target.isDefined ) msg.target.get else msg.sender)

  }

//  private def defaultErrorHandler( e : Any ) {
//    println( e )
//    SVarActor.shutdownSystem()
//  }

  addHandler[TriggerOnGroup] {
    msg =>
      this.syncOn( msg.syncGroup, (syncGroup : SyncGroup)  => { self ! PerformSimulationStep() } )

  }

  protected var beginOfLastSimulationStep = -1L
  protected var endOfLastSimulationStep = -1L
  protected var currentStart = System.currentTimeMillis()

  private var performingSimulation = false

  private var postponedTrigger = List[PerformSimulationStep]()

  addHandler[PerformSimulationStep] {
    msg => {
      if( !performingSimulation ) {
        frequency match {
          case p : Passive =>
          case _ =>
            this.startSimulation()
        }
      } else {
        postponedTrigger = postponedTrigger ::: msg :: Nil
      }

    }
  }

  def startSimulation() {
    if( beginOfLastSimulationStep < 0 ) beginOfLastSimulationStep = System.currentTimeMillis()
    this.currentStart = System.currentTimeMillis()
    this.performingSimulation = true
    for( a <- triggerOnStepBegin ) a ! PerformSimulationStep()
    this.performSimulationStep()
  }

  protected def simulationCompleted() {
    require( this.performingSimulation, "This message can only be called if the component is currently performing a simulation step!" )
    this.stepCount = this.stepCount + 1
    for( a <- stepCountObserver ) a ! CurrentStepCount( stepCount )
    for( a <- triggerOnStepEnd ) a ! PerformSimulationStep()
    this.performingSimulation = false

    frequency match {
      case f : Unbound =>
        self ! PerformSimulationStep()

      case f : Hard =>
        val nextAt = this.currentStart + ((1/f.hz)*1000).asInstanceOf[Long]
        if( nextAt < System.currentTimeMillis() )
          this.startSimulation()
        else
          this.addJobAt( nextAt ){ startSimulation() }

      case f : Soft =>
        val nextAt = this.currentStart + ((1/f.hz)*1000).asInstanceOf[Long]
        if( nextAt < System.currentTimeMillis() )
          self ! PerformSimulationStep()
        else
          this.addJobAt( nextAt) { self ! PerformSimulationStep() }

      case f : Weak =>
        var nextAt = this.currentStart + ((1/f.hz)*1000).asInstanceOf[Long]
        while( nextAt < System.currentTimeMillis() )
          nextAt = nextAt + ((1/f.hz)*1000).asInstanceOf[Long]
        this.addJobAt( nextAt) { self ! PerformSimulationStep() }

      case f : Triggered =>

    }

    this.beginOfLastSimulationStep = this.currentStart
    this.endOfLastSimulationStep = System.currentTimeMillis()
    for( trigger <- this.postponedTrigger ) self ! trigger
    postponedTrigger = List()
    this.worldStepComplete()
  }

  /**
   * Called for each simulation step the component should execute. The freqency with which this method is called
   * depends on the used [[simx.core.component.ExecutionStrategy]].
   */
  protected def performSimulationStep()

  // register this component
  ComponentManagementActor ! RegisterComponent( self, componentType, componentName )
  WorldInterfaceActor ! ComponentRegisterRequest( componentName, self )
}


//! Exception which is thrown if a requested component was not registered yet
class ComponentDoesNotExistException(val name : Symbol) extends Exception{
  override def toString = "ComponentDoesNotExistException: " + name.name
}
//! Exception which is thrown if a requested component was not registered yet
class ComponentTypeDoesNotExistException(val name : Symbol) extends Exception{
  override def toString = "ComponentTypeDoesNotExistException: " + name.name
}


/**
 * @author Stephan Rehfeld
 */
trait ComponentHandling extends SVarActor {


  private[component] var componentForNameHandler = Map[java.util.UUID,(Option[SVarActor.Ref])=>Unit]()
  private[component] var componentForTypeHandler = Map[java.util.UUID,(Option[List[SVarActor.Ref]])=>Unit]()

  final protected def componentForName( componentName : Symbol, handler : ((Option[SVarActor.Ref]) => Unit ) ) {
    val uuid = java.util.UUID.randomUUID()
    ComponentManagementActor ! GetComponentForName( componentName, uuid )
    componentForNameHandler = componentForNameHandler + (uuid -> handler)
  }

  final protected def componentForType( componentType : GroundedSymbol, handler : (Option[List[SVarActor.Ref]]) => Unit  ) {
    val uuid = java.util.UUID.randomUUID()
    ComponentManagementActor ! GetComponentForType( componentType, uuid )
    componentForTypeHandler = componentForTypeHandler + (uuid -> handler)
  }

  final protected def registerComponent( component : SVarActor.Ref, componentType : GroundedSymbol, componentName : Symbol ) {
    ComponentManagementActor ! RegisterComponent( component, componentType, componentName )
  }

  addHandler[ComponentForNameAnswer] {
    msg =>
      val handler = componentForNameHandler( msg.uuid )
      componentForNameHandler = componentForNameHandler - msg.uuid
      handler( msg.component )
  }

  addHandler[ComponentForTypeAnswer] {
    msg =>
      val handler = componentForTypeHandler( msg.uuid )
      componentForTypeHandler = componentForTypeHandler - msg.uuid
      handler( msg.components )
  }

}

/**
 * @author Stephan Rehfeld
 */
case class GetComponentForName( componentName : Symbol, uuid : java.util.UUID )
                              ( implicit @transient actor : SVarActor.Ref ) extends SimXMessage

/**
 * @author Stephan Rehfeld
 */
case class ComponentForNameAnswer( component : Option[SVarActor.Ref], uuid : java.util.UUID )
                                 ( implicit @transient actor : SVarActor.Ref ) extends SimXMessage

/**
 * @author Stephan Rehfeld
 */
case class GetComponentForType( componentType : GroundedSymbol, uuid : java.util.UUID )(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage

/**
 * @author Stephan Rehfeld
 */
case class ComponentForTypeAnswer( components : Option[List[SVarActor.Ref]], uuid : java.util.UUID )
                                 (implicit @transient actorContext : SVarActor.Ref) extends SimXMessage


case class InterManagementActorGetComponentForName( componentName : Symbol, uuid : java.util.UUID )
                                                  (implicit @transient actorContext : SVarActor.Ref) extends SimXMessage

case class InterManagementActorComponentForNameAnswer(  component : Option[SVarActor.Ref], uuid : java.util.UUID )
                                                     (implicit @transient actorContext : SVarActor.Ref) extends SimXMessage

case class InterManagementActorGetComponentForType( componentType : GroundedSymbol, uuid : java.util.UUID )
                                                  (implicit @transient actorContext : SVarActor.Ref) extends SimXMessage
case class InterManagementActorComponentForTypeAnswer( components : Option[List[SVarActor.Ref]], uuid : java.util.UUID )
                                                     (implicit @transient actorContext : SVarActor.Ref) extends SimXMessage

/**
 * @author Stephan Rehfeld
 */
case class RegisterComponent( component : SVarActor.Ref, componentType : GroundedSymbol, componentName : Symbol )
                            (implicit @transient actorContext : SVarActor.Ref) extends SimXMessage
case class ComponentManagementActorInCluster( componentManagementActor : SVarActor.Ref )
                                            (implicit @transient actorContext : SVarActor.Ref) extends SimXMessage

/**
 * @author Dennis Wiebusch
 */
abstract class SingletonActor[T <: SVarActor](ctor: => T, val name : String ){
  private[component] var instance : Option[SVarActor.Ref] = None
  def self : SVarActor.Ref = instance match {
    case Some(ref) => ref
    case None =>
      synchronized {
        if (instance.isEmpty)
          instance = Some( SVarActor.createActor(ctor, Some( name )) )
        instance.get
      }
  }

  override def toString: String =
    name + " (Singleton)"

  def !(msg : Any){
    self ! msg
  }
}

abstract  class SingletonComponent[T <: Component](ctor: => T, val componentType : GroundedSymbol, val componentName : Symbol)
  extends SingletonActor[T](ctor, componentName.name){
  override def self : SVarActor.Ref = instance match {
    case Some(ref) => ref
    case None =>
      val ref = super.self
      ComponentManagementActor ! RegisterComponent( ref, componentType, componentName )(instance.get)
      ref
  }
}

/**
 * @author Stephan Rehfeld
 */
object ComponentManagementActor extends SingletonActor(new ComponentManagementActor, "componentManagementActor")

class ComponentManagementActor extends SVarActor {
  //! the registered components by name
  private val registeredComponents = mutable.Map[Symbol, SVarActor.Ref]()
  //! the registered components by type
  private var componentsByType = Map[GroundedSymbol, List[SVarActor.Ref]]()

  private var foreignClusterManagementActors = List[SVarActor.Ref]()

  private var requests = Map[java.util.UUID,Int]()
  private var senderCache = Map[java.util.UUID,SVarActor.Ref]()
  private var componentsLists = Map[java.util.UUID,List[SVarActor.Ref]]()

  addHandler[ComponentManagementActorInCluster] {
    msg => {
      println("Got foreign component manager: " + msg.componentManagementActor)
      foreignClusterManagementActors = foreignClusterManagementActors ::: msg.componentManagementActor :: Nil
    }
  }

  addHandler[GetComponentForName] {
    msg =>
      if( foreignClusterManagementActors.isEmpty || registeredComponents.contains( msg.componentName ) ) {
        msg.sender ! ComponentForNameAnswer( registeredComponents.get(msg.componentName), msg.uuid)
      } else {
        requests = requests + (msg.uuid -> 0)
        senderCache = senderCache + (msg.uuid -> msg.sender)
        for( a <- foreignClusterManagementActors ) {
          a ! InterManagementActorGetComponentForName( msg.componentName, msg.uuid )
          println( "Sending request for " + msg.componentName + " to " + a )
        }
      }
  }

  addHandler[InterManagementActorGetComponentForName] {
    msg => {
      println("Got request, answering with " + registeredComponents.get(msg.componentName) )
      msg.sender ! InterManagementActorComponentForNameAnswer( registeredComponents.get(msg.componentName), msg.uuid)
    }
  }

  addHandler[InterManagementActorComponentForNameAnswer] {
    msg => {
      if( msg.component.isDefined ) {
        senderCache( msg.uuid ) ! ComponentForNameAnswer( msg.component, msg.uuid)
        senderCache = senderCache - msg.uuid
        requests = requests - msg.uuid
      } else {
        if( senderCache.contains( msg.uuid ) ) {
          requests = requests + (msg.uuid -> (requests( msg.uuid) + 1))
          if( requests( msg.uuid ) == foreignClusterManagementActors.size ) {
            senderCache( msg.uuid ) ! ComponentForNameAnswer( None, msg.uuid)
            senderCache = senderCache - msg.uuid
            requests = requests - msg.uuid
          }
        }
      }
    }
  }

  addHandler[GetComponentForType] {
    msg => {
      if( foreignClusterManagementActors.isEmpty ) {
        msg.sender ! ComponentForTypeAnswer( componentsByType.get(msg.componentType), msg.uuid)
      } else {
        requests = requests + (msg.uuid -> 0)
        senderCache = senderCache + (msg.uuid -> msg.sender)
        componentsLists = componentsLists + (msg.uuid -> componentsByType.getOrElse( msg.componentType, List() ) )
        for( a <- foreignClusterManagementActors ) {
          a ! InterManagementActorGetComponentForType( msg.componentType, msg.uuid )
          println( "Sending request for " + msg.componentType + " to " + a )
        }
      }
    }
  }

  addHandler[InterManagementActorGetComponentForType] {
    msg =>
      println("Got request, answering with " + componentsByType.get(msg.componentType) )
      msg.sender ! InterManagementActorComponentForTypeAnswer( componentsByType.get(msg.componentType), msg.uuid )
  }

  addHandler[InterManagementActorComponentForTypeAnswer] {
    msg =>
      requests = requests + ( msg.uuid -> (requests( msg.uuid ) + 1))
      componentsLists = componentsLists + (msg.uuid -> (componentsLists(msg.uuid) ::: msg.components.getOrElse( List() ) ) )
      if( requests( msg.uuid ) == foreignClusterManagementActors.size ) {
        senderCache( msg.uuid ) ! ComponentForTypeAnswer( if( componentsLists( msg.uuid ).isEmpty ) None else Some( componentsLists( msg.uuid ) ), msg.uuid)
        requests = requests - msg.uuid
        componentsLists = componentsLists - msg.uuid
        senderCache = senderCache - msg.uuid
      }
  }

  addHandler[RegisterComponent] {
    msg =>
      if (registeredComponents.get(msg.componentName).isEmpty){
        componentsByType = componentsByType.updated(msg.componentType, msg.component :: componentsByType.getOrElse(msg.componentType, Nil) )
        registeredComponents += msg.componentName -> msg.component
      }
  }
}