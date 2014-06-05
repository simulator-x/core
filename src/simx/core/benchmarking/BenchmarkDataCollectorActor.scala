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

import simx.core.svaractor.{SVarActor, SimXMessage}
import java.text.SimpleDateFormat
import java.util.{Date, UUID}
import java.io.{FileOutputStream, File}
import simx.core.component.SingletonActor

abstract class SimXBenchmarkMessage( implicit @transient actor : SVarActor.Ref )  extends SimXMessage

case class RegisterActor( time : Long )( implicit @transient actor : SVarActor.Ref )  extends SimXBenchmarkMessage
case class MessageEnqueueMeasurement( messageClass : Class[_], data : (SVarActor,UUID,Long,Long,Long) )( implicit @transient actor : SVarActor.Ref )  extends SimXMessage

object BenchmarkDataCollectorActor extends SingletonActor(new BenchmarkDataCollectorActor, "benchmarkDataCollector" )

class BenchmarkDataCollectorActor extends SVarActor {

  private var registeredActors = List[(SVarActor.Ref,Long)]()
  private var nonSVarActorMessage = List[(String,Class[_],SVarActor,UUID,Long,Long,Long)]()

  if( CoreBenchmarkingConfiguration.enableBenchmarking &&  CoreBenchmarkingConfiguration.enableSaveBenchmarkDataToDisk) Runtime.getRuntime.addShutdownHook( new Thread( new Runnable() {
    def run() {

      val currentDate = new Date
      val dateFormat = new SimpleDateFormat( "yyy-MM-dd--HH-mm-ss")
      val currentDateString = dateFormat.format( currentDate )
      val directory =  "benchmarkresults" + File.separator +  currentDateString

      val benchmarkDirectory = new File( directory )
      benchmarkDirectory.mkdirs()

      val journal = new FileOutputStream( "benchmarkresults" + File.separator +  currentDateString + File.separator + "journal" )

      var actorId = 0
      journal.write( "actor type;creation time;id\n".getBytes )


      for( (actor,time) <- registeredActors ) {
        journal.write( (actor.getClass + ";" + time + ";" + actorId + '\n').getBytes  )

        val sending = new FileOutputStream( "benchmarkresults" + File.separator +  currentDateString + File.separator + actorId + "-sending" )

        val messageSendData = actor.asInstanceOf[SVarActor].messageSendData
        sending.write( "message type;target actor;uuid;creation;begin;end\n".getBytes )
        for( (msgType,data) <- messageSendData ) {
          for( (target,uuid,creation,begin,end) <- data ) {
            sending.write((msgType + ";" + target + ";" + uuid + ";" + creation + ";" + begin + ";" + end + '\n').getBytes )
          }

        }

        sending.close()

        val handler = new FileOutputStream( "benchmarkresults" + File.separator +  currentDateString + File.separator + actorId + "-handler" )

        handler.write( "message type;uuid;creation;begin;end\n".getBytes )
        val messageProcessingLog = actor.asInstanceOf[SVarActor].messageProcessingLog
        for( (msgType, data) <- messageProcessingLog ) {
          for( (uuid,creation,begin,end) <- data ) {
            handler.write(  (msgType + ";" + uuid + ";" + creation + ";" + begin + ";" + end + '\n').getBytes )
          }
        }

        handler.close()

        actorId = actorId + 1
      }

      journal.close()

      val nonSVarActorsSending = new FileOutputStream( "benchmarkresults" + File.separator +  currentDateString + File.separator + "nonSVarActors-sending" )

      nonSVarActorsSending.write( "sender;message type;target;uuid;creation,begin;end\n".getBytes)
      for( (sender,messageType,target,uuid,creation,begin,end)<- nonSVarActorMessage ) {
        nonSVarActorsSending.write( (sender + ";" + messageType + ";" + target + ";" + uuid + ";" + creation + ";" + begin + ";" + end + '\n' ).getBytes )
      }

      nonSVarActorsSending.close()
    }
  }) )

  if( CoreBenchmarkingConfiguration.enableBenchmarking && CoreBenchmarkingConfiguration.enableBenchmarkDataCollectorActor ) {
    println( "Starting bechmarking data collector actor" )
  }

  addHandler[ RegisterActor ] {
    msg =>
      registeredActors = registeredActors ::: (msg.sender,msg.time) :: Nil
  }



  addHandler[ MessageEnqueueMeasurement ] {
    msg =>
      if( CoreBenchmarkingConfiguration.enableWarnIfNonSVarActorSentAMessage ) println( "A non SVarActor with the type " + msg.sender + " sent a message to an SVarActor!")
      val (target,messageId,creation,begin,end) = msg.data
      this.nonSVarActorMessage = this.nonSVarActorMessage ::: (sender.toString(),msg.messageClass,target,messageId,creation,begin,end) :: Nil
  }
}