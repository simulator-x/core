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

package simx.core.svaractor.benchmarking

import simx.core.benchmarking.{RegisterActor, MessageEnqueueMeasurement, BenchmarkDataCollectorActor, CoreBenchmarkingConfiguration}
import simx.core.svaractor.{SVarActorBase, SimXMessage}

/**
 * User: dwiebusch
 * Date: 23.02.13
 * Time: 15:16
 */
trait ActorBenchmarking extends SVarActorBase{
  val creationTime =
    if( CoreBenchmarkingConfiguration.enableBenchmarking &&
      CoreBenchmarkingConfiguration.enableBenchmarkDataCollectorActor &&
      CoreBenchmarkingConfiguration.enableSVarActorRegistration &&
      BenchmarkDataCollectorActor != null  )
    {
      System.nanoTime()
    } else
      0L

  protected def registerActorForBenchmark() {
    BenchmarkDataCollectorActor ! RegisterActor( creationTime )(self)
  }



  override def preStart() {
    if( CoreBenchmarkingConfiguration.enableBenchmarking && CoreBenchmarkingConfiguration.enableBenchmarkDataCollectorActor && CoreBenchmarkingConfiguration.enableSVarActorRegistration && BenchmarkDataCollectorActor != null  )
      this.registerActorForBenchmark()
    startUp()
  }



  private def benchmark[T](msg : Any)( function : () => T) {
    val enqueBegin =  if( CoreBenchmarkingConfiguration.enableBenchmarking && CoreBenchmarkingConfiguration.enableSIRISMessageEnqueBeginTimeLogging ) System.nanoTime() else 0
    function.apply()
    val enqueEnd = if( CoreBenchmarkingConfiguration.enableBenchmarking && CoreBenchmarkingConfiguration.enableSIRISMessageEnqueEndTimeLogging ) System.nanoTime() else 0
    if( CoreBenchmarkingConfiguration.enableBenchmarking && (CoreBenchmarkingConfiguration.enableSIRISMessageEnqueBeginTimeLogging || CoreBenchmarkingConfiguration.enableSIRISMessageEnqueEndTimeLogging  ) && !msg.isInstanceOf[MessageEnqueueMeasurement] ) {
      val messageId = if( msg.isInstanceOf[SimXMessage]) msg.asInstanceOf[SimXMessage].messageUUID
      else {
        if( CoreBenchmarkingConfiguration.enableBenchmarking && CoreBenchmarkingConfiguration.enableWarnIfNonSIRISMessageWasSent ) println( "A non SimXMessage with type " + msg + " was sent to an SVarActor!" )
        null
      }
      val data = (actorContext,messageId,if( msg.isInstanceOf[SimXMessage]) msg.asInstanceOf[SimXMessage].creationTimestamp else -1l,enqueBegin,enqueEnd)
      val messageClass = msg.asInstanceOf[AnyRef].getClass
      val sVarActor = actorContext
      if( sVarActor.messageSendData.contains( messageClass ) ) {
        sVarActor.messageSendData = sVarActor.messageSendData + (messageClass -> (sVarActor.messageSendData(messageClass) ::: data :: Nil) )
      } else {
        sVarActor.messageSendData = sVarActor.messageSendData + (messageClass -> (data :: Nil) )
      }

    }
  }

}
