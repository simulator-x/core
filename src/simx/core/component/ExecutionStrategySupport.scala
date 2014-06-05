/*
 * Copyright 2013 The SIRIS Project
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

import simx.core.svaractor.{SimXMessage, SVarActor}
import simx.core.svaractor.synclayer.{SyncGroupHandling, SyncGroupLeader, SyncGroup}
import scala.annotation.meta.param

/**
 * Date: 20.11.13
 * Time: 18:55
 */
trait ExecutionStrategySupport extends SVarActor with SyncGroupLeader with SyncGroupHandling{
  /**
   * Called for each simulation step the component should execute. The frequency with which this method is called
   * depends on the used [[simx.core.component.ExecutionStrategy]].
   */
  protected def performSimulationStep()


  protected var frequency : Frequency = Passive()

  protected var triggerOnStepBegin = List[SVarActor.Ref]()
  protected var triggerOnStepEnd = List[SVarActor.Ref]()

  protected var stepCount : Long = 0
  protected var stepCountObserver = Set[SVarActor.Ref]()

  protected var beginOfLastSimulationStep = -1L
  protected var endOfLastSimulationStep = -1L
  protected var currentStart = System.currentTimeMillis()

  private var performingSimulation = false

  private var postponedTrigger = List[PerformSimulationStep]()

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

  addHandler[TriggerOnGroup] {
    msg =>
      this.syncOn( msg.syncGroup, (syncGroup : SyncGroup)  => { self ! PerformSimulationStep() } )

  }

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


}


/**
 * This message triggers a component to perform the next simulation step.
 *
 * @author Stephan Rehfeld
 */
case class PerformSimulationStep()(implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage

/**
 * This message sets the frequency of a component.
 *
 * @author Stephan Rehfeld
 *
 * @param frequency The frequency of the component.
 */
case class SetFrequency( frequency : Frequency )(implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage


/**
 * This message is sent to an observer of the stepcount after a component finished the simulation step.
 *
 * @author Stephan Rehfeld
 *
 * @param stepCount The step count.
 */
case class CurrentStepCount( stepCount : Long )(implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage

/**
 * The sender of this message gets notified about the current step count.
 *
 * @author Stephan Rehfeld
 *
 * @param target the target
 *
 */
case class ObserveStepCount( target : Option[SVarActor.Ref] = None )(implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage

/**
 * The sender of this message will not be notified about changes of the step count an more.
 *
 * @author Stephan Rehfeld
 *
 * @param target the target
 *
 */
case class IgnoreStepCount( target : Option[SVarActor.Ref] = None  )(implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage

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
case class SetTrigger( trigger : List[(SVarActor.Ref,Boolean)] )(implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage {

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
case class TriggerOnGroup( syncGroup : SyncGroup )(implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage {
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
}