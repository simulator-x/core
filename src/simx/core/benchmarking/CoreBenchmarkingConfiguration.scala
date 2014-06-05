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

package simx.core.benchmarking

/**
 * Created by IntelliJ IDEA.
 * User: stephan_rehfeld
 * Date: 01.10.11
 * Time: 18:33
 * To change this template use File | Settings | File Templates.
 */

object CoreBenchmarkingConfiguration {

  /**
   * This attribute indicates if the benchmarking is switched on or off.
   * If this attribute is false, the benchmarking is completely disabled even if other attributes in this
   * object are switched on.
   */
  val enableBenchmarking = false



  /**
   * This attribute indicates if the data collection actor for benchmarking should be started.
   */
  val enableBenchmarkDataCollectorActor = true

  /**
   * This attribute indicated if all SVarActors should register them self at the data collection actor.
   * They only register them self if the data collection actor is activated.
   */
  val enableSVarActorRegistration = true

  /**
   * This attribute indicates if all SIRIS Messages should log the timestamp of their creation.
   */
  val enableSIRISMessageCreationTimeLogging = true

  val enableSIRISMessageEnqueBeginTimeLogging = true

  val enableSIRISMessageEnqueEndTimeLogging = true

  val enableSIRISMessageUUID = true

  val enableWarnIfNonSIRISMessageWasSent = false

  val enableWarnIfNonSVarActorSentAMessage = false

  val enableHandlerBeginTimeLogging = true

  val enableHandlerEndTimeLogging = true

  val enableSaveBenchmarkDataToDisk = true

}