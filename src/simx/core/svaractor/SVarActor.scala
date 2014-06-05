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

import benchmarking.ActorBenchmarking
import handlersupport.HandlerBenchmarking
import simx.core.helper.{JVMTools, Loggable}
import akka.actor.{Address, Deploy, ActorSystem, Props}
import simx.core.entity.typeconversion.ConvertedSVar
import collection.mutable
import ref.WeakReference
import com.typesafe.config.ConfigFactory
import simx.core.clustering.ClusterSubSystem
import reflect.runtime.universe.TypeTag
import akka.remote.RemoteScope
import scala.reflect.ClassTag


/**
 * This is the companion object of the SVarActor, an actor that can handle
 * State Variables.
 */
object SVarActor {
  type Ref = akka.actor.ActorRef

  private val tickDuration = if (JVMTools.isWindows) 10 else 1

  private val missing = "missing"
  private val configString =
    if(System.getProperty("simx.remote.hostname", missing) != missing)
      """
      akka {
        scheduler {
          tick-duration = """ + tickDuration + """
        }
        actor {
          provider = "akka.remote.RemoteActorRefProvider"
        }
        remote {
          enabled-transports = ["akka.remote.netty.tcp"]
          netty.tcp {
            hostname = """" + System.getProperty("simx.remote.hostname") + """"
            port = """ + System.getProperty("simx.remote.port", "9000") + """
          }
        }
      }
      """
    else
      "akka.scheduler.tick-duration=" + tickDuration

  var config = ConfigFactory.parseString(configString).withFallback(ConfigFactory.load())
  var system = ActorSystem("SVarActor", config)

  def createActor[ T <: SVarActor ](ctor : => T, name : Option[String] = None ) : Ref = {
    if( name.isDefined )
      system.actorOf(Props(ctor), name.get )
    else
      system.actorOf(Props(ctor) )
  }

  def shutdownSystem(){
    system.shutdown()
  }
}

/**
 * This trait is an actor that can handle State Variables and fullfills the
 * required notifications.
 */
trait SVarActor extends SVarActorBase with SVarFunctions with HandlerBenchmarking with ActorBenchmarking with Loggable{
  protected final implicit val actorContext = this
  //
  //
  //
  // inner classes
  //
  //



  private def defaultErrorHandler( e : Exception ) {
    e.printStackTrace()
    System.exit( -100 )
  }

  final protected def createActor[T <: SVarActor]( constructorCall : => T, targetNode : Option[Symbol] = None )
                                                 ( handler      : SVarActor.Ref => Unit )
                                                 ( errorHandler :  Exception => Unit = defaultErrorHandler ) = {
    val actor =
      if( targetNode.isEmpty || !ClusterSubSystem.getKnown.contains( targetNode.get ) ) {
        context.actorOf(Props(constructorCall))
      } else {
        val (interface, port) = ClusterSubSystem.getKnown( targetNode.get )
        context.actorOf( Props( constructorCall).withDeploy( Deploy( scope = RemoteScope( Address( "akka", "SimX", interface, port ) ) ) ) )
      }
    try {
      ask(actor, ActorCreation()){
        answer : ActorCreation => handler(actor)
      }
    } catch {
      case e : Exception =>
        errorHandler( e )
    }
    actor
  }



  /**
   *  This method sets a new value to the given State Variable. If the
   * State Variable is owned by the calling Actor the value gets written
   * immediately. If the state variable is owned by another actor, the other
   * actor get an advice to write the value, but it depends on it's own logic
   * if the value gets written or not.
   *
   * The method always returns immediately and never blocks.
   *
   * @param value The new value for the State Variable
   */
  final protected[svaractor] def set[T](sVar : SVar[T], value: T)  {
    sVar match {
      case convertedSVar : ConvertedSVar[_,T] =>
        convertedSVar.set(value)
      case _ =>
        if (owner( sVar ) == self)
          write(self, sVar, value )
        else
          owner( sVar ) ! WriteSVarMessage( self, sVar, value )
    }
  }

  // Retrieve the current value. The supplied handler is used only once.
  /**
   * This method reads the current value of the State Variable. It does not
   * block and returnd immediately. The given consumer function at the parameter
   * gets called when the value has been provided. The consumer is only valid
   * one time and gets deleted after the value has been provided.
   *
   * The given handler is processed in the current actor that is calling the
   * method.
   *
   * If the State Variable belongs to the current actor, the value can be read
   * immediately. In that case the consumer function is processed immediately
   * and the get method returns after the consumer function has been completed.
   *
   * @param consume A function that consumes the value of the State Variable.
   */
  protected[svaractor] def get[T : ClassTag](sVar : SVar[T])( consume: T => Any )  {
    sVar match {
      case convertedSVar : ConvertedSVar[_,_] =>
        convertedSVar.get(consume)
      case _ =>
        if( self == owner( sVar ) )
          consume ( read( sVar ) )
        else {
          owner( sVar ) ! ReadSVarMessage( sVar )
          addSingleUseHandler[ValueOfSVarMessage[T]]( {
            case ValueOfSVarMessage( svar, value) if svar == sVar => consume(value)
          } : PartialFunction[ValueOfSVarMessage[T], Unit])
        }
    }
  }

  // self observes future value changes. The supplied handle is reused.
  /**
   * Calling this method will observe the given state variable. Every time the
   * value of the State Variable gets changed the given handler messages gets
   * called. The handler message is running in the actor that called the
   * observe method.
   *
   * Only one handler can be registered at one time. If the method gets called
   * again within the the same actor the old handler gets replaced.
   *
   * An actor can observe the own state variable.
   *
   * A change of the value is only be notified if the value really change. E.g.
   * a State Variable contains the value 1 and a write operation with the value 1
   * is performed, no observers will be notified, because the value has not changed.
   *
   * @param handler The handler, that gets called when the value of the State Variable has changed.
   */
  protected[svaractor] def observe[T](sVar : SVar[T])( handler: T => Any)  {
    observe(sVar, Set[SVarActor.Ref]())(handler)
  }

  // self observes future value changes. The supplied handle is reused.
  /**
   * Calling this method will observe the given state variable. Every time the
   * value of the State Variable gets changed the given handler messages gets
   * called. The handler message is running in the actor that called the
   * observe method.
   *
   * Only one handler can be registered at one time. If the method gets called
   * again within the the same actor the old handler gets replaced.
   *
   * An actor can observe the own state variable.
   *
   * A change of the value is only be notified if the value really change. E.g.
   * a State Variable contains the value 1 and a write operation with the value 1
   * is performed, no observers will be notified, because the value has not changed.
   *
   * @param handler The handler, that gets called when the value of the State Variable has changed.
   * @param ignoredWriters Value changes by SVarActors contained in this set are ignored.
   */
  protected[svaractor] def observe[T](sVar : SVar[T], ignoredWriters: Set[SVarActor.Ref] = Set())(handler: T => Any)  {
    sVar match {
      case convertedSVar : ConvertedSVar[_, T] =>
        convertedSVar.observe(handler, ignoredWriters)
      case _ =>
        if ( self == owner( sVar) )
          addObserver( sVar, self, ignoredWriters )
        else
          owner( sVar ) ! ObserveSVarMessage( sVar, self, ignoredWriters )
        addSVarObserveHandler(sVar, handler.asInstanceOf[(Any => Unit)])
    }
  }

  /**
   *  This method returns the last known owner of the state variable.
   *
   */
  final protected[svaractor] def owner[T](sVar : SVar[T] ) : SVarActor.Ref = {
    sVar match {
      case convertedSVar : ConvertedSVar[_,_] =>
        owner( convertedSVar.wrappedSVar )
      case _ => getOrUpdateSVarOwner(sVar, sVar.initialOwner)
    }
  }

  // Assign a new owner.
  /**
   * This method assigns a new owner to the state variable. It does not block
   * and returns immediately. An owner change can be rejected by the current
   * owner.
   *
   * @param newOwner The new owner of the State Variable. Must not be null.
   */
  final protected[svaractor] def owner[T](sVar : SVar[T], newOwner: SVarActor.Ref) {
    val currentOwner = owner( sVar )

    if(self == currentOwner)
      changeOwner(self, sVar, newOwner)
    else
      currentOwner ! ChangeOwnerOfSVarMessage( sVar, newOwner )
  }

  // Stop observing this SVar.
  /**
   * Calling this method stops observing the State Variable. The registred
   * handler gets removed and the current actor is not informed about changes
   * of the value any more.
   */
  protected[svaractor] def ignore[T](sVar : SVar[T] )  {
    sVar match {
      case convertedSVar : ConvertedSVar[_,_] =>
        ignore( convertedSVar.wrappedSVar )
      case _ =>
        owner( sVar ) ! IgnoreSVarMessage( sVar, self )
        removeSVarObserveHandlers( sVar )
    }

  }

  abstract class Request[U]{
    private var r : Option[U => Unit] = None
    def send( receiver : SVarActor.Ref, answer : U => Unit ) {
      r = Some(answer)
      receiver ! this
    }

    def reply( answer : U ){
      r.getOrElse(throw new Exception("reply function was never set")).apply(answer)
    }
  }

  var messageSendData : Map[Class[_],List[(SVarActor,java.util.UUID,Long,Long,Long)]] = Map() // (target,message id, beginEnque, endEnque)

  protected class ChangeOwnerStatus(val svar : SVar[_], val newOwner : SVarActor.Ref) {
    private def clearQueueFor( actor : SVarActor.Ref )(implicit context : SVarActor) {
      heldMessages.get( actor ) match {
        case None =>
        case Some(queue) => {
          heldMessages -= actor
          // forward messages if the current actor was the sender, return to sender otherwise
          if (actor == context)
            queue.foreach( newOwner ! _ )
          else
            actor ! HeldMessagesMessage( svar, newOwner, queue.toList)(self)
        }
      }
    }

    def apply( actor : SVarActor.Ref) : Option[mutable.Queue[SVarMessage]] =
      heldMessages.get(actor)

    def acknowledgeBy( actor : SVarActor.Ref ) =
      if (isAccepted) clearQueueFor( actor ) else heldAcknowledges += actor

    def pushMessage( msg : SVarMessage  ) {
      val queue = heldMessages.getOrElse(msg.sender, mutable.Queue[SVarMessage]())
      queue += msg
      heldMessages.update(msg.sender, queue)
    }

    def acceptChange() = {
      changeAccepted = true
      heldAcknowledges.foreach( a => clearQueueFor( a ) )
      heldAcknowledges.clear()
      acknowledgeBy(self)
    }

    def isAccepted = changeAccepted

    private val heldMessages     = mutable.Map[SVarActor.Ref, mutable.Queue[SVarMessage]]()
    private val heldAcknowledges = mutable.Set[SVarActor.Ref]()
    private var changeAccepted   = false
  }

  //
  //
  // variable initialization
  //
  //

  private val data = mutable.WeakHashMap[SVar[_],SVarDataImpl[_]]()

  //! the map which holds queues for each svar that is beeing transferred
  private val heldMessages = mutable.Map[SVar[_], ChangeOwnerStatus]()

  //! the locally known owners
  private val sVarOwners = mutable.Map[SVar[_], SVarActor.Ref]()

  //!
  protected var isRunning : Boolean = true

  protected val sVarObserveHandlers = mutable.Map[SVar[_], Any => Unit]()

  //
  //
  // package private methods (at least most of them)
  //
  //

  //! !!! DO NOT USE THIS FUNCTION BUT THE SVARS OWNER() FUNCTION UNLESS YOU KNOW EXACTLY WHAT YOU ARE DOING !!!
  private[svaractor] def getLocallyKnownSVarOwner(svar: SVar[_]): Option[SVarActor.Ref] =
    sVarOwners.get(svar)

  private[svaractor] def getOrUpdateSVarOwner(svar: SVar[_], newOwner : SVarActor.Ref) : SVarActor.Ref =
    sVarOwners.getOrElseUpdate(svar, newOwner)

  private[svaractor] def addSVarObserveHandler(svar : SVar[_], handler : Any => Unit ) {
    sVarObserveHandlers.update(svar, handler)
  }

  private[svaractor] def removeSVarObserveHandlers(svar : SVar[_]) =
    sVarObserveHandlers -= svar

  private[svaractor] def addSVarOwner(svar: SVar[_], owner : SVarActor.Ref) {
    sVarOwners += svar -> owner
  }

  private[svaractor] def changeOwner[T](sender    : SVarActor.Ref,
                                        svar      : SVar[T],
                                        newOwner  : SVarActor.Ref,
                                        value     : Option[T] = None) : Boolean = {
    if (self == newOwner){
      if (!this.isOwnerOf(svar)) value match {
        case Some(_data) => insertSVar(svar, _data)
        case None => throw new Exception
      }
      sender ! AcceptSVarMessage(svar)
    }
    else if (isOwnerOf(svar)) heldMessages.get(svar) match {
      case Some(changeOwnerStatus) if changeOwnerStatus.isAccepted => owner(svar, newOwner ) //!!! is this correct? mustn't we tell "ownerHasChanged"?
      case Some(changeOwnerStatus) => changeOwnerStatus.pushMessage(ChangeOwnerOfSVarMessage( svar, newOwner))
      case None =>
        heldMessages(svar) = new ChangeOwnerStatus(svar, newOwner)
        newOwner ! BunchOfSimXMessagesMessage(
          OfferSVarMessage( getOriginal(svar), read(svar)) ::  moveObservers(svar, newOwner)
        )
    }
    //if nothing matched, the svar was unknown, so we return false
    else
      return false
    //if we got here, everything is ok, so we return true
    true
  }

  protected def moveObservers[T](svar : SVar[T], newOwner : SVarActor.Ref) : List[SimXMessage] =
    data.get( svar ) match {
      case Some(_data) =>
        (for ( (observer, ignoredWriters) <- _data.getObservers) yield
          ObserveSVarMessage( svar, self, ignoredWriters)).toList
      case None => Nil
    }

  protected def getSVarsObservedBy( observer : SVarActor.Ref) =
    data.filter( pair => pair._2.isObservedBy( observer) )

  protected def getObserversOf( svar : SVar[_] ) = data.get(svar) match {
    case Some(svardata) => svardata.getObservers
    case None => Map[SVarActor.Ref, Set[SVarActor.Ref]]()
  }

  /**
   * Creates a new SVar.
   * The data of this SVar is stored.
   * The weak reference that is stored with the data is used detrmine, if
   * the data is not needed any more.
   */
  private[svaractor] def createSVar[T]( value: T )( implicit manifest : ClassTag[T]) : SVar[T] =
    insertSVar(new SVarImpl(self, manifest), value)

  private def insertSVar[T]( sVar: SVar[T], value: T ) : SVar[T] = {
    data       += sVar -> new SVarDataImpl( value, new WeakReference( sVar ) )
    sVarOwners += sVar -> self
    sVar
  }

  //  private def removeSVar( svar : SVar[_] ) = {
  //    removeSVarOwner(svar)
  //    removeSVarData(svar)
  //  }

  private def removeSVarData( sVar: SVar[_] ) =
    data -= sVar

  private[svaractor] final def write[T]( writer: SVarActor.Ref, sVar: SVar[T], value: T ) {
    heldMessages.get(sVar) match {
      case Some(changeOwnerStatus) => changeOwnerStatus.pushMessage( WriteSVarMessage( writer, sVar, value) )
      case None => if( !read( sVar ).equals( value ) )
        internalWrite( writer, sVar, value )
    }
  }

  protected def internalWrite[T]( writer: SVarActor.Ref, sVar: SVar[T], value: T ) {
    data( sVar ).write( writer, value )
  }

  private[svaractor] def update[T]( writer : SVarActor.Ref, sVar : SVar[T], updateMethod : T => T ) {
    write(writer, sVar, updateMethod(read(sVar)))
  }

  private[svaractor] def read[T]( sVar: SVar[T] ) : T =
    data( sVar ).read.asInstanceOf[T]

  private[svaractor] final def addObserver( sVar: SVar[_], a: SVarActor.Ref, ignoredWriters: Set[SVarActor.Ref] ) {
    heldMessages.get(sVar) match {
      case Some(changeOwnerStatus) => changeOwnerStatus.pushMessage( ObserveSVarMessage( sVar, self, ignoredWriters))
      case None => internalAddObserver( sVar, a, ignoredWriters )
    }
  }

  protected def internalAddObserver( sVar: SVar[_], a: SVarActor.Ref, ignoredWriters: Set[SVarActor.Ref] ) {
    data( sVar ).addObserver( a, ignoredWriters )
  }

  //
  //
  // private methods
  //
  //

  protected final def removeObserver( sVar: SVar[_], a: SVarActor.Ref ) {
    heldMessages.get(sVar) match {
      case Some(changeOwnerStatus) => changeOwnerStatus.pushMessage( IgnoreSVarMessage( sVar, self) )
      case None => internalRemoveObserver(sVar, a)
    }
  }

  protected def internalRemoveObserver(sVar: SVar[_], a: SVarActor.Ref ) {
    data( sVar ).removeObserver( a )
  }

  private def getOriginal[T](svar : SVar[T]) : SVar[T] = data.get(svar) match {
    case Some(_data) => _data.svar.get match {
      case Some(refToOrig) => refToOrig.asInstanceOf[SVar[T]]
      case _ => throw InvalidWeakRefException
    }
    case None => throw NotSVarOwnerException
  }

  private def isOwnerChangeInProgress( svar : SVar[_] ) : Boolean =
    heldMessages.get(svar).isDefined

  private def isOwnerOf(svar : SVar[_]) : Boolean =
    data.get(svar).isDefined

  private def updateSVarOwner(svar : SVar[_], newOwner : SVarActor.Ref) {
    sVarOwners.update(svar, newOwner)
  }

  private def handleOwnerDependentMsg[T <: SVarHoldingMessage[_]]( handler : T => Unit )( msg : T ) {
    if ( isOwnerChangeInProgress(msg.sVar) )
      changeOwnerInProgessHandler( msg )
    else if ( isOwnerOf(msg.sVar) )
      handler( msg )
    else
      msg.sender ! SVarOwnerChangedMessage( msg.sVar, owner( msg.sVar ), msg )
  }

  private def createValueOfSVarMsg[T](sVar : SVar[T]) : ValueOfSVarMessage[T] =
    ValueOfSVarMessage( sVar, read(sVar))


  //  private def removeSVarOwner(svar : SVar[_]) {
  //    sVarOwners -= svar
  //  }

  //
  //
  //  public methods
  //
  //


  /**
   *
   */
  def shutdown() {
    SVarActor.system.stop(self)
    if (self != actorContext.self){
      self ! Shutdown()(self)
    } else {
      //isRunning = false
      //TODO: implement this
    }
  }

  final def handleMessage : PartialFunction[Any, Any] =
    handlersAsPF orElse {case _ => () }
  //------------------------------------//
  //                                    //
  //    handler definition section      //
  //                                    //
  //------------------------------------//

  private def changeOwnerInProgessHandler( msg : SVarHoldingMessage[_]) {
    heldMessages.get(msg.sVar) match {
      case None => throw OwnerChangeNotInProgressException
      case Some(changeOwnerStatus) =>
        if (changeOwnerStatus(msg.sender).isEmpty)
          msg.sender ! SVarOwnerChangeInProgressMessage( msg.sVar, changeOwnerStatus.newOwner)
        changeOwnerStatus.pushMessage(msg)
        heldMessages.update(msg.sVar, changeOwnerStatus)
    }
  }

  addHandler[OfferSVarMessage[Any]]{
    msg => changeOwner( msg.sender, msg.sVar, self, Some( msg.value ) )
  }

  addHandler[Shutdown]{
    msg => shutdown()
  }


  addHandler[SVarOwnerChangeInProgressMessage[_]]{ msg =>
    if(heldMessages.get(msg.sVar).isEmpty)
      heldMessages(msg.sVar) = new ChangeOwnerStatus(msg.sVar, msg.newOwner)
    msg.sender ! AcknowledgeMessage( SVarOwnerChangeInProgressMessage( msg.sVar, msg.newOwner))
  }

  addHandler[AcknowledgeMessage]{ msg =>
    msg.refMessage match {
      case SVarOwnerChangeInProgressMessage( svar, newOwner) => heldMessages.get(svar) match {
        case Some(changeOwnerStatus) => changeOwnerStatus.acknowledgeBy( msg.sender )
        case None =>
      }
    }
  }

  addHandler[AcceptSVarMessage[_]]{ msg =>
    updateSVarOwner(msg.sVar, msg.sender)
    removeSVarData(msg.sVar)
    heldMessages.get(msg.sVar) match {
      case Some(changeOwnerStatus) => changeOwnerStatus.acceptChange()
      case None =>
    }
  }

  addHandler[HeldMessagesMessage[_]]{ msg =>
    updateSVarOwner(msg.sVar, msg.newOwner)
    msg.msgs.foreach( applyHandlers( _ ) )

    //Handle msgs stored after the acknowledge was sent
    heldMessages.get(msg.sVar) collect {
      case changeOwnerStatus =>
        heldMessages.remove( msg.sVar )
        changeOwnerStatus(self).collect {
          case queue => queue.foreach( applyHandlers( _ ) )
        }
    }
  }

  addHandler[ChangeOwnerOfSVarMessage[_]]{ msg =>
    if (isOwnerChangeInProgress(msg.sVar))
      changeOwnerInProgessHandler( msg )
    else {
      val successful = changeOwner(msg.sender, msg.sVar, msg.newOwner)
      if (! successful) getLocallyKnownSVarOwner(msg.sVar) match {
        case Some(owner) => msg.sender ! SVarOwnerChangedMessage( msg.sVar, owner, msg)
        case _           => msg.sender ! UnknownSVarMessage(msg.sVar, msg)
      }
    }
  }

  addHandler[CreateSVarMessage[_]] { msg =>
    msg.sender ! SVarCreatedMessage( SVarImpl.apply[Any]( msg.value ), msg.asInstanceOf[CreateSVarMessage[Any]], getManifest( msg.value ) )
  }

  addHandler[ReadSVarMessage[_]]{
    handleOwnerDependentMsg( msg => msg.sender ! createValueOfSVarMsg( msg.sVar ) )
  }

  addHandler[WriteSVarMessage[Any]]{
    handleOwnerDependentMsg( msg => write( msg.writer, msg.sVar, msg.value ) )
  }

  addHandler[ObserveSVarMessage[_]]{
    handleOwnerDependentMsg( msg => addObserver(msg.sVar, msg.observer, msg.ignoredWriters) )
  }

  addHandler[IgnoreSVarMessage[_]]{
    handleOwnerDependentMsg( msg => removeObserver(msg.sVar, msg.observer) )
  }

  addHandler[NotifyWriteSVarMessage[_]] { msg =>
    sVarObserveHandlers get msg.sVar collect { case handler => handler( msg.value ) }
  }

  addHandler[SVarOwnerChangedMessage[_]] { msg =>
    sVarOwners += msg.sVar -> msg.newOwner
    msg.newOwner ! msg.originalMessage
  }

  addHandler[BunchOfSimXMessagesMessage]{
    msg => msg.msgs.foreach( applyHandlers(_) )
  }

  protected def addJobIn( in : Long )( job : => Unit ) {
    addJobAt(System.currentTimeMillis() + in)(job)
  }
}

trait SVarFunctions{
  protected[svaractor] def set[T](sVar : SVar[T], value: T)
  protected[svaractor] def get[T : ClassTag](sVar : SVar[T])( consume: T => Any)
  protected[svaractor] def observe[T](sVar : SVar[T])( handler: T => Any)
  protected[svaractor] def observe[T](sVar : SVar[T], ignoredWriters: Set[SVarActor.Ref] = Set())(handler: T => Any)
  protected[svaractor] def ignore[T](sVar : SVar[T] )
  protected[svaractor] def owner[T](sVar : SVar[T], newOwner: SVarActor.Ref)
  protected[svaractor] def owner[T](sVar : SVar[T] ) : SVarActor.Ref
}