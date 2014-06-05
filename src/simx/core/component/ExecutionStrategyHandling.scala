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

import simx.core.svaractor.SVarActor

/**
 * This trait provides a function to apply a [[simx.core.component.ExecutionStrategy]].
 *
 * @author Stephan Rehfeld
 */
trait ExecutionStrategyHandling {

  /**
   * This method interprets and starts a given execution strategy. After this message is called, all components are
   * running according to the given execution strategy.
   *
   * @param executionStrategy The execution strategy.
   */
  def start( executionStrategy : ExecutionStrategy )(implicit actorContext : SVarActor.Ref) {
    require( executionStrategy != null, "The parameter 'executionStrategy' must not be 'null'!" )
    val frequencies = executionStrategy.getFrequencies
    require( frequencies.size > 0, "At least one frequency must be defined!" )
    for( f <- frequencies ) f._1 ! SetFrequency( f._2 )

    val trigger = executionStrategy.getTriggerList
    for ( f <- trigger ) {
      f._1 match {
        case Left( actor ) =>
          actor ! SetTrigger( f._2 )
        case Right( syncGroup ) =>
          for( (actor,_) <- f._2 ) actor ! TriggerOnGroup( syncGroup )

      }

    }

    val startWith = executionStrategy.getStartSet
    for ( a <- startWith ) a ! PerformSimulationStep()
  }



}
