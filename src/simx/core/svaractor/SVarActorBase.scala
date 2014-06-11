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

package simx.core.svaractor

import concurrent.duration._
import akka.actor._
import handlersupport.HandlerSupportImpl
import java.util.concurrent.TimeUnit
import collection.mutable
import akka.actor.Terminated
import reflect.{classTag, ClassTag}
import reflect.runtime.universe.TypeTag

/**
 * User: dwiebusch
 * Date: 23.02.13
 * Time: 15:41
 */
case object ActorCreation


protected case class ReplyRequest[T, U](id : java.util.UUID, sender : SVarActor.Ref, msg : T, manifest : ClassTag[U])

protected trait AnswerType

trait ReplyContext{
  def apply(value : Any) : AnswerType
}

protected[svaractor] object SVarActorDeadWatch{
  private case class RegisterSVarActor(ref : SVarActor.Ref)
  private class DeadWatch extends Actor {
    private var registeredActors = Set[SVarActor.Ref]()

    def receive = {
      case RegisterSVarActor(ref) =>
        context.watch(ref)
        registeredActors = registeredActors + ref
      case Terminated(ref) =>
        registeredActors = registeredActors - ref
        if (registeredActors.isEmpty){
          SVarActor.shutdownSystem()
          ActorSystem("SVarActorDeadWatch").shutdown()
        }
      case _ => //ignore
    }
  }

  private val self =
    ActorSystem("SVarActorDeadWatch").actorOf(Props(new DeadWatch))

  def register(ref : SVarActor.Ref){
    self ! RegisterSVarActor(ref)
  }
}

trait SVarActorContext[ActorType <: SVarActor]{
  /**
   * reference to this
   */
  //protected implicit val actorContext : ActorType
}


trait SVarActorBase extends Actor with SVarActorContext[SVarActor] with HandlerSupportImpl{
  val printWarnings = true

  protected implicit val actorContext : this.type = this

  context.setReceiveTimeout(Duration.create(16, TimeUnit.MILLISECONDS))
  //SVarActorDeadWatch.register(self)

  /**
   * Returns false if the actor was stopped, true if it is running
   * @return false if the actor was stopped
   */
  protected def isRunning : Boolean

  /**
   * Defines the function used for message handling
   * @return a function that is used for message handling
   */
  protected def handleMessage : PartialFunction[Any, Any]

  /**
   * called when the actor is started
   */
  protected def startUp() {}

  private var jobs  = mutable.PriorityQueue[(Long, () => Unit)]()(new Ordering[(Long, () => Unit)]{
    def compare(x: (Long, () => Unit), y: (Long, () => Unit)) = (y._1 - x._1).toInt
  })

  protected def addJobAt( executionTime : Long )( job :  => Unit) {
    jobs += executionTime ->  { () => job }
    updateTimeout()
  }

  protected def ask[T : TypeTag : ClassTag](send : ReplyRequest[Any, T] => Unit, msg : Any)(replyHandler : T => Any) {
    val id = java.util.UUID.randomUUID
    actorContext.addSingleUseHandlerPF[(java.util.UUID, T)]({ case (`id`, value) => replyHandler(value) } )
    send(ReplyRequest(id, self, msg, classTag[T]))
  }

  def ask[T : TypeTag : ClassTag](receiver : ActorSelection, msg : Any)(replyHandler : T => Any){
    ask[T](receiver ! _, msg)(replyHandler)
  }

  def ask[T : TypeTag : ClassTag](receiver : SVarActor.Ref, msg : Any)(replyHandler : T => Any) {
    ask[T](receiver ! _, msg)(replyHandler)
  }

  def waitUntilProcessed(receiver : SVarActor.Ref, msg : Any)(replyHandler : => Unit){
    ask[Any](receiver ! _, msg)(_ => replyHandler)
  }

  private var replyContext : ReplyContext =
    UnknownReplyContext

  protected case object DelayedAnswer extends AnswerType
  private case class UnknownReceiver(msg : String) extends AnswerType

  protected def delayedReplyWith[T]( createResult : (T => Any) => Any )
                                   ( handleResult : T => Any = (v : T) => v ) : AnswerType =
  {
    val rContext = replyContext
    createResult{ result => rContext(handleResult(result)) }
    DelayedAnswer
  }

  protected def provideAnswer[T] : T => AnswerType = {
    val rContext = replyContext
    rContext.apply
  }

  private case object UnknownReplyContext extends ReplyContext{
    def apply(value : Any) = {
      println(UnknownReceiver("ERROR: " + this + " replying '"+ value + "' to unknown sender"))
      UnknownReceiver("ERROR: " + this + " replying '"+ value + "' to unknown sender")
    }
  }

  private case class DefinedReplyContext(origin : SVarActor.Ref, id : java.util.UUID) extends ReplyContext {
    def apply(value : Any) = {
      origin ! (id, value)
      DelayedAnswer
    }
  }

  protected def getAnswerTo : PartialFunction[Any, Any] = {
    case ActorCreation =>
      ActorCreation
    case msg =>
      handleMessage(msg)
  }

  final def receive = {
    case ReceiveTimeout =>
      if (isRunning){
        var current = System.currentTimeMillis()
        while( jobs.nonEmpty && jobs.head._1 <= current ) {
          jobs.head._2.apply()
          jobs = jobs.tail
          current = System.currentTimeMillis()
        }
        updateTimeout()
      }
    case ReplyRequest(id, sender, msg, manifest) =>
      replyContext = DefinedReplyContext(sender, id)
      val answer = getAnswerTo(msg)
      if (answer != DelayedAnswer){
        if (manifest.runtimeClass.isInstance(answer))
          sender ! (id, answer)
        // TODO: Remove this later
        else if (printWarnings){
          if (answer == ((): Unit))
            println(this + " provided () as an answer to " + msg +
              ", this behaviour is deprecated. please change your code to return DelayedAnswer" +
              " (either using provideAnswer or delayedReplyWith)")
          else
            println(this + " provided bad answer of type " + answer.getClass.getCanonicalName + " to " + msg + "! " +
              "Make sure that " + manifest.runtimeClass.getCanonicalName + " or DelayedAnswer is returned" )
        }
      }
      replyContext = UnknownReplyContext
      updateTimeout()
    case msg =>
      handleMessage(msg)
      updateTimeout()
  }

  /**
   * resets the timeout for the next message
   */
  protected def updateTimeout(){
    val now = System.currentTimeMillis()
    if (jobs.isEmpty)
      context.setReceiveTimeout(Duration.Undefined)
    else if (jobs.head._1 > now)
      context.setReceiveTimeout((jobs.head._1 - now) milliseconds)
    else
      self ! ReceiveTimeout
  }

  /**
   * calls the startUp method (redirects akka call)
   */
  override def preStart() {
    startUp()
  }
}
