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

import benchmarking.MessageBenchmarking
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag

/*
* @todo DOCUMENT THIS FILE
*/

abstract class SimXMessage(implicit val sender : SVarActor.Ref) extends Serializable with MessageBenchmarking

abstract class SVarMessage(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage

abstract class SVarHoldingMessage[T](implicit @transient actorContext : SVarActor.Ref) extends SVarMessage{
  def sVar : SVar[T]
}

case class AcknowledgeMessage( refMessage: SVarMessage )(implicit @transient actorContext : SVarActor.Ref) extends SVarMessage

case class CreateSVarMessage[T]( value: T )(implicit @transient actorContext : SVarActor.Ref) extends SVarMessage
case class SVarCreatedMessage[T]( sVar: SVar[T], createMessage: CreateSVarMessage[T], manifest : ClassTag[_])(implicit @transient actorContext : SVarActor.Ref)
  extends SVarHoldingMessage[T]

case class ReadSVarMessage[T]( sVar: SVar[T] )(implicit @transient actorContext : SVarActor.Ref) extends SVarHoldingMessage[T]
case class ValueOfSVarMessage[T]( sVar: SVar[T], value: T )(implicit @transient actorContext : SVarActor.Ref) extends SVarHoldingMessage[T]

case class WriteSVarMessage[T]( writer: SVarActor.Ref, sVar: SVar[T], value: T )(implicit @transient actorContext : SVarActor.Ref)
  extends SVarHoldingMessage[T]

case class ObserveSVarMessage[T]( sVar: SVar[T], observer : SVarActor.Ref, ignoredWriters: Set[SVarActor.Ref] )
                                (implicit @transient actorContext : SVarActor.Ref)
  extends SVarHoldingMessage[T]

case class IgnoreSVarMessage[T]( sVar: SVar[T], observer : SVarActor.Ref )(implicit @transient actorContext : SVarActor.Ref) extends SVarHoldingMessage[T]

case class NotifyWriteSVarMessage[T]( sVar: SVar[T], value: T )(implicit @transient actorContext : SVarActor.Ref) extends SVarHoldingMessage[T]

case class ChangeOwnerOfSVarMessage[T]( sVar: SVar[T], newOwner: SVarActor.Ref )(implicit @transient actorContext : SVarActor.Ref)
  extends SVarHoldingMessage[T]

case class OfferSVarMessage[T]( sVar: SVar[T], value: T )(implicit @transient actorContext : SVarActor.Ref) extends SVarHoldingMessage[T]
case class AcceptSVarMessage[T]( sVar: SVar[T] )(implicit @transient actorContext : SVarActor.Ref) extends SVarHoldingMessage[T]
case class SVarOwnerChangeInProgressMessage[T]( sVar: SVar[T], newOwner : SVarActor.Ref )(implicit @transient actorContext : SVarActor.Ref)
  extends SVarHoldingMessage[T]
case class HeldMessagesMessage[T]( sVar : SVar[T], newOwner : SVarActor.Ref, msgs : List[SVarMessage])(implicit @transient actorContext : SVarActor.Ref)
  extends SVarHoldingMessage[T]

case class SVarOwnerChangedMessage[T]( sVar: SVar[T], newOwner: SVarActor.Ref, originalMessage: SVarMessage )(implicit @transient actorContext : SVarActor.Ref)
  extends SVarHoldingMessage[T]

case class UnknownSVarMessage[T]( sVar: SVar[T], originalMessage: SVarMessage )(implicit @transient actorContext : SVarActor.Ref)
  extends SVarHoldingMessage[T]


case class Shutdown( )(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage

case class ObserveMessage( )(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage
case class IgnoreMessage(  )(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage
case class SyncMessage(  )(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage
case class BunchOfSimXMessagesMessage( msgs : List[SimXMessage])(implicit @transient actorContext : SVarActor.Ref) extends SimXMessage



class SVarMessages
