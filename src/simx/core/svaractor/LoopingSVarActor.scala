/*
 * Copyright 2015 The SIRIS Project
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

package simx.core.svaractor

import simx.core.component.PerformSimulationStep

/**
 * Created by
 * martin
 * in August 2015.
 */
abstract class LoopingSVarActor extends SVarActor {

  private var lastStepStartingTimeInMillis = 0L
  protected var framePeriodInMillis = 64L

  protected def performLoopStep(deltaTinSeconds: Float)

  /**
   * Sends a message to this actor (self) to trigger the start of the loop.
   */
  protected def startLoop() {
    self ! PerformSimulationStep()
  }

  /**
   * Defines the reaction to receiving the PerformSimulationStep message:

   * Calculate deltaT and other times, call performLoopStep, schedule the sending of the next
   * PerformSimulationStep message.
   */
  addHandler[PerformSimulationStep]{ msg =>
    val currentStepStartingTimeInMillis = System.currentTimeMillis

    var deltaTinMillis =
      if(lastStepStartingTimeInMillis == 0) framePeriodInMillis
      else currentStepStartingTimeInMillis - lastStepStartingTimeInMillis

    if(deltaTinMillis > 8 * framePeriodInMillis) {
      println("[LoopingSVarActor] warn: deltaT to high (" + deltaTinMillis + "ms).")
      deltaTinMillis = 8 * framePeriodInMillis
    }
    performLoopStep(deltaTinMillis.toFloat / 1000f)

    lastStepStartingTimeInMillis = currentStepStartingTimeInMillis
    val nextStepStartingTimeInMillis = currentStepStartingTimeInMillis + framePeriodInMillis

    if(nextStepStartingTimeInMillis < System.currentTimeMillis) self ! PerformSimulationStep()
    else addJobAt(nextStepStartingTimeInMillis){self ! PerformSimulationStep()}
  }
}
