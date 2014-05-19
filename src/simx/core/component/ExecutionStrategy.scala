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

import java.io.{ObjectInputStream, ObjectOutputStream}
import simx.core.svaractor.synclayer.SyncGroup
import simx.core.svaractor.SVarActor

/**
 * This is a small domain specific language to express an execution strategy for components within Simulator X. A
 * [[simx.core.component.Frequency]] can be set for every component. If components are [[simx.core.component.Triggered]]
 * a triggering order can be configured.
 *
 * This is a small example from a more complex application. The physics triggers the renderer after a new simulation
 * step is completed. The renderer triggers the physics if it begins the render the next frame, so the physics and
 * the renderer runs in parallel. The artificial intelligence is triggered by the renderer. The steering that process
 * signals from input devices and moves the player characters runs with 30Hz.
 *
 * {{{
    val es = ExecutionStrategy where
      renderer runs Triggered() and
      physics runs Triggered() and
      steering runs Soft( 30 ) and
      ai runs Triggered() where
        ai isTriggeredBy Left(physics) onStepComplete() and
        renderer isTriggeredBy Left( physics ) onStepComplete() and
        physics isTriggeredBy Left( renderer ) onStepBegin() startWith( Set() + physics )
   }}}
 *
 * @author Stephan Rehfeld
 */
object ExecutionStrategy {

  /**
   * The starting point for a new execution strategy. This method takes an actor as parameter and returns an helper
   * object to set the [[simx.core.component.Frequency]].
   *
   * @param component The component which frequency should be set.
   * @return An helper object to set the [[simx.core.component.Frequency]].
   */
  def where( component : SVarActor.Ref ) = {
    require( component != null, "The parameter 'component' must not be 'null'!" )
    val executionStrategy = ExecutionStrategy( List(), List(), Set() )
    ComponentToFrequency( executionStrategy, component, Passive() )
  }

}

/**
 * This class represents a execution strategy. It's constructed using the DSL.
 *
 * @param components A list of components and [[simx.core.component.Frequency]] of the components.
 * @param triggerDescriptions A list and configures who triggers who.
 * @param startSet A set that contains all components that should be triggered initially.
 */
case class ExecutionStrategy private[component]( components : List[ComponentToFrequency], triggerDescriptions : List[TriggerDescription], startSet : Set[SVarActor.Ref] ) {
  require( components != null, "The parameter 'components' must not be 'null'!" )
  require( triggerDescriptions != null, "The parameter 'triggerDescriptions' must not be 'null'!" )
  require( startSet != null, "The parameter 'startSet' must not be 'null'!" )

  /**
   * This method adds one more component to the execution strategy. It takes to component as parameter and returns
   * an helper object to set the [[simx.core.component.Frequency]].
   *
   * @param component The component which frequency should be set.
   * @return An helper object to set the [[simx.core.component.Frequency]].
   */
  def and( component : SVarActor.Ref ) = ComponentToFrequency( this, component, Passive() )

  /**
   * This method creates a trigger description in the execution strategy.
   *
   * @param component The component that is triggered by some other component or a
   *                  [simx.core.svaractor.synclayer.SyncGroup.
   * @return A helper object to construct the trigger description.
   */
  def where( component : SVarActor.Ref ) = TriggerDescription( this, component, Left(component), triggerOnBegin = false  )

  /**
   * This method is used by the [[simx.core.component.ExecutionStrategyHandling]] trait to interpret the execution
   * strategy.
   *
   * @return A set that contains all components and their frequencies.
   */
  private[component] def getFrequencies = {
    val d = for( c <- components ) yield (c.component, c.frequency)
    d.toSet
  }

  /**
   * This method is used by the [[simx.core.component.ExecutionStrategyHandling]] trait to interpret the execution
   * strategy.
   *
   * @return A map that contains all components, who they are trigger, and when they are doing it.
   */
  private[component] def getTriggerList = {
    val d = for( t <- triggerDescriptions ) yield (t.trigger -> List[(SVarActor.Ref,Boolean)]())
    var m = d.toMap
    for( t <- triggerDescriptions ) {
      m = m + (t.trigger -> (m( t.trigger ) ::: (t.component, t.triggerOnBegin) :: Nil ) )
    }
    m
  }

  /**
   * This method is used by the [[simx.core.component.ExecutionStrategyHandling]] trait to interpret the execution
   * strategy.
   *
   * @return This method returns a set of all components that will receive a
   *         [[simx.core.component.PerformSimulationStep]] on start up.
   */
  private[component] def getStartSet = {
    val filtered = ( getFrequencies.filter( (x) => {
      x._2 match {
        case t : Triggered =>
          false
        case p : Passive =>
          false
        case _ =>
          true
      }
    } ) )
    val s = for ( t <- filtered ) yield t._1
    startSet | s
  }

}

/**
 * This is a small helper object to construct an [[simx.core.component.ExecutionStrategy]]. It ties together a component
 * an a [[simx.core.component.Frequency]].
 *
 * @author Stephan Rehfeld
 *
 * @param executionStrategy The parent [[simx.core.component.ExecutionStrategy]].
 * @param component The actor which [[simx.core.component.Frequency]] should be set.
 * @param frequency The [[simx.core.component.Frequency]] of the component.
 */
private[component] case class ComponentToFrequency( executionStrategy : ExecutionStrategy, component : SVarActor.Ref, frequency : Frequency ) {
  require( executionStrategy != null, "The parameter 'executionStrategy' must not be 'null'!" )
  require( component != null, "The parameter 'component' must not be 'null'!" )
  require( frequency != null, "The parameter 'frequency' must not be 'null'!" )

  /**
   * This method sets the [[simx.core.component.Frequency]] of the component.
   * @param f The [[simx.core.component.Frequency]] for the component.
   * @return The altered [[simx.core.component.ExecutionStrategy]].
   */
  def runs( f : Frequency ) = ExecutionStrategy( executionStrategy.components ::: ComponentToFrequency( executionStrategy, component, f ) :: Nil, executionStrategy.triggerDescriptions, executionStrategy.startSet  )

}

/**
 * This is a small helper object to construct an [[simx.core.component.ExecutionStrategy]]. It configures who triggers
 * a component and when it is triggered.
 *
 * @param executionStrategy The parent [[simx.core.component.ExecutionStrategy]].
 * @param component The component that triggered.
 * @param trigger The trigger, an actor or a [[simx.core.svaractor.synclayer.SyncGroup]]
 * @param triggerOnBegin If the trigger is an actor, should be trigger be sent at the begin of the step?
 */
private[component] case class TriggerDescription( executionStrategy : ExecutionStrategy, component : SVarActor.Ref, trigger : Either[SVarActor.Ref,SyncGroup], triggerOnBegin : Boolean ) {

  require( executionStrategy != null, "The parameter 'executionStrategy' must not be 'null'!" )
  require( component != null, "The parameter 'component' must not be 'null'!" )
  require( trigger != null, "The parameter 'trigger' must not be 'null'!" )

  /**
   * The trigger of the component. Either an actor or a [[simx.core.svaractor.synclayer.SyncGroup]]
   * @param c The actor or [[simx.core.svaractor.synclayer.SyncGroup]] that triggers the component.
   * @return An altered trigger description.
   */
  def isTriggeredBy( c : Either[SVarActor.Ref,SyncGroup] ) = TriggerDescription( executionStrategy, component, c, triggerOnBegin )

  /**
   * This method sets if the trigger should be sent a the begin of a simulation step.
   *
   * @return An altered trigger description.
   */
  def onStepBegin() = TriggerDescription( executionStrategy, component, trigger, triggerOnBegin = true )

  /**
   * This method sets if the trigger should be sent a the begin of a simulation step.
   *
   * @return An altered trigger description.
   */
  def onStepComplete() = TriggerDescription( executionStrategy, component, trigger, triggerOnBegin = false )

  /**
   * This method and one more trigger description to the execution strategy.
   *
   * @param component The component that is triggered by some other component or a [[simx.core.svaractor.synclayer.SyncGroup]].
   *
   * @return A new trigger description.
   */
  def and( component : SVarActor.Ref ) = TriggerDescription( ExecutionStrategy( executionStrategy.components, executionStrategy.triggerDescriptions ::: this :: Nil, executionStrategy.startSet ), component, Left(component), triggerOnBegin = false  )

  /**
   * This method sets a set of components with frequency [[simx.core.component.Triggered]] that should receive a
   * [[simx.core.component.PerformSimulationStep]] message on start up.
   *
   * @param components A set of components that should receive a
   *                   [[simx.core.component.PerformSimulationStep]] message on start up
   * @return The altered execution strategy.
   */
  def startWith( components : Set[SVarActor.Ref] ) = ExecutionStrategy( executionStrategy.components, executionStrategy.triggerDescriptions ::: this :: Nil, components )

}
