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

package simx.core.svaractor

import handlersupport.HandlerSupport

/**
 * A ping message that can be used track the reaction time of an actor.
 *
 * The trait [[simx.core.svaractor.PingableActor]] can be used to make an actor "pingable". This trait creates
 * a [[simx.core.svaractor.Reply]] message.
 *
 * @author Stephan Rehfeld
 *
 * @param identifier An arbitrary identifier.
 * @param timestamp A time stamp, when the ping was sent. Is filled with the result of [[java.lang.System.nanoTime()]]
 *                  by default.
 */
case class Ping( identifier : Any, timestamp : Long = System.nanoTime())
               (implicit @transient actor : SVarActor.Ref) extends SimXMessage

/**
 * This message is sent as a reply to a [[simx.core.svaractor.Ping]].
 *
 * @author Stephan Rehfeld
 *
 * @param identifier The original identifier.
 * @param originalTimestamp The original time stamp.
 * @param replyTimestamp  A time stamp, when the reply was sent. Is filled with the result of
 *                        [[java.lang.System.nanoTime()]] by default.
 */
case class Reply( identifier : Any, originalTimestamp : Long, replyTimestamp : Long = System.nanoTime() )
                (implicit @transient actor : SVarActor.Ref) extends SimXMessage

/**
 * Mixing in this trait gives an actor to answer on [[simx.core.svaractor.Ping]] with a [[simx.core.svaractor.Reply]].
 *
 * @author Stephan Rehfeld
 */
trait PingableActor extends HandlerSupport with SVarActor{
  addHandler[Ping]( {
    msg =>
      msg.sender ! Reply( msg.identifier, msg.timestamp )
  })

}

/**
 * This trait adds a handler for [[simx.core.svaractor.Reply]] messages. The timespan between sending and receiving
 * is printed to the console.
 *
 * @author Stephan Rehfeld
 */
trait PingReplyOutputActor extends HandlerSupport {
  addHandler[Reply]( {
    msg =>
      val time = System.nanoTime()
      println( "Reply for \"" + msg.identifier + "\". source->source=" + ((time - msg.originalTimestamp)/1000000000.0) + "s, source->target=" + ((msg.replyTimestamp - msg.originalTimestamp)/1000000000.0) + "s, target->source=" + ((time - msg.replyTimestamp)/1000000000.0) + "s." )
  })
}