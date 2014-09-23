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

import akka.actor._
import akka.remote.RemoteScope
import akka.remote.DisassociatedEvent
import com.typesafe.config.{Config, ConfigFactory}
import simx.core.entity.description.SVal.SValType
import simx.core.helper.{GarbageCollectionObserver, JVMTools, Loggable}
import simx.core.entity.typeconversion.{TypeInfo, ConvertedSVar}
import simx.core.clustering.ClusterSubSystem
import simx.core.entity.description.SVal
import simx.core.svaractor.TimedRingBuffer._
import scala.collection.mutable
import scala.ref.WeakReference
import scala.reflect.ClassTag


/**
 * This is the companion object of the SVarActor, an actor that can handle
 * State Variables.
 */
object SVarActor {
  type Ref = akka.actor.ActorRef
  protected[core] type Address = akka.actor.Address
  protected[core] def addressOf(ref : Ref) : Address = ref.path.address
  protected[core] def isLocal(ref : Ref)   : Boolean = SVarActor.addressOf(ref).hasLocalScope

  private var systemName = "SVarActor"
  private var hostname = System.getProperty("simx.remote.hostname", "")
  private val port =  System.getProperty("simx.remote.port", "0")
  private var system : Option[ActorSystem] = None
  private var config : Option[Config] = None
  private var profiling = false

  private def configString =
    if(isRemotingEnabled)
      """
      akka {
        scheduler {
          tick-duration = """ + JVMTools.minTickDuration + """
        }
        actor {
          provider = "akka.remote.RemoteActorRefProvider"
        }
        remote {
          enabled-transports = ["akka.remote.netty.tcp"]
          netty.tcp {
            hostname = """" + hostname + """"
            port = """ + port  + """
          }
        }
      }
                                 """
    else {
      if( profiling ) {
        "akka.scheduler.tick-duration=" + JVMTools.minTickDuration + "\n" + "akka.actor.provider = \"akka.actor.profiling.LocalProfilingActorRefProvider\""
      } else
        """
        akka {
          log-dead-letters = 1
          scheduler.tick-duration=""" + JVMTools.minTickDuration + """
        }
                                                                   """
    }

  private def getConfig : Config =
    config.getOrElse( ConfigFactory.parseString(configString).withFallback(ConfigFactory.load()))

  protected def getAddress = {
    require(system.nonEmpty, "System has to be instantiated before calling getAddress function")
    class Ext(s: ExtendedActorSystem) extends Extension{ def getAddress = s.provider.getDefaultAddress }
    object ExtKey extends ExtensionKey[Ext]
    ExtKey.apply(getSystem).getAddress
  }

  protected def getAddressOf(ref : SVarActor.Ref) =
    ref.path.toStringWithAddress(getAddress)

  protected def getSystem = {
    if (system.isEmpty)
      system = Some(ActorSystem(systemName, getConfig))
    system.get
  }

  protected def subscribeAkkaEvents[T](c : Class[T], actor :  SVarActor.Ref){
    getSystem.eventStream.subscribe(actor, c)
  }

  def isRemotingEnabled : Boolean =
    hostname.nonEmpty

  def setSystemName(name : String){
    require(system.isEmpty, "SystemName may be defined before starting the first actor")
    systemName = name
  }

  def setProfiling( profiling : Boolean ) {
    this.profiling = profiling
  }

  def setHostName(hostName : String){
    require(system.isEmpty, "Hostname may be defined before starting the first actor")
    hostname = hostName
  }

  def setSystemConfig(cfg : Config){
    require(system.isEmpty, "Config may be defined before starting the first actor")
    config = Some(cfg)
  }

  def getHostname : Option[String] =
    if (hostname.nonEmpty) Some(hostname) else None

  def getSystemName =
    systemName

  def createActor( props : Props, name : Option[String] ) : Ref = {
    val sys = getSystem
    if( name.isDefined )
      sys.actorOf(props, name.get )
    else
      sys.actorOf(props, props.actorClass().getSimpleName + "-" + java.util.UUID.randomUUID())
  }

  def createActor[ T <: SVarActor : ClassTag ](ctor : => T, name : Option[String] = None ) : Ref =
    createActor(Props.apply(ctor), name)

  def shutdownSystem(){
    system.foreach(_.shutdown())
  }

  def shutdownActor(ref : Ref)(implicit sender : Ref){
    ref ! Shutdown()
  }
}

/**
 * This trait is an actor that can handle State Variables and fullfills the
 * required notifications.
 */
trait SVarActor extends SVarActorBase with SVarFunctions with Loggable{

  //
  //
  //
  // inner classes
  //
  //

  override def preStart() {
    subscribeAkkaEvents(classOf[DisassociatedEvent])
    super.preStart()
  }

  private def defaultErrorHandler( e : Exception ) {
    e.printStackTrace()
    System.exit( -100 )
  }

  final protected def spawnActor[T <: SVarActor : ClassTag]( constructorCall : => T, targetNode : Option[Symbol] = None ) =
    createActor(constructorCall, targetNode)(_ => {})()

  final private def _createActor(props : Props, targetNode : Option[Symbol] = None )
                                ( handler      : SVarActor.Ref => Unit )
                                ( errorHandler :  Exception => Unit = defaultErrorHandler ) = {
    val actor =
      if( targetNode.isEmpty || !ClusterSubSystem.getKnown.contains( targetNode.get ) ) {
        context.actorOf(props, props.actorClass().getSimpleName + "-" + java.util.UUID.randomUUID())
      } else {
        val (interface, port) = ClusterSubSystem.getKnown( targetNode.get )
        context.actorOf(
          props.withDeploy( Deploy( scope = RemoteScope( Address( "akka", SVarActor.systemName, interface, port ) ) ) ),
          props.actorClass().getSimpleName + "-" + java.util.UUID.randomUUID()
        )
      }
    try {
      ask[Any](actor, ActorCreation){
        case ActorCreation => handler(actor)
      }
    } catch {
      case e : Exception =>
        errorHandler( e )
    }
    actor
  }

  final protected def createActor( props : Props )( handler : SVarActor.Ref => Unit )
                                 ( errorHandler :  Exception => Unit) =
    _createActor(props)(handler)(errorHandler)

  final protected def createActor[T <: SVarActor : ClassTag]( constructorCall : => T, targetNode : Option[Symbol] = None )
                                                            ( handler      : SVarActor.Ref => Unit )
                                                            ( errorHandler :  Exception => Unit = defaultErrorHandler ) =
    _createActor(Props.apply(constructorCall), targetNode)(handler)(errorHandler)

  protected def subscribeAkkaEvents[T](c : Class[T]){
    SVarActor.subscribeAkkaEvents(c, self)
  }

  protected def getAddressOf(ref : SVarActor.Ref) : String =
    SVarActor.getAddressOf(ref)

  protected def getPort : Option[Int] =
    SVarActor.getAddress.port


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
  override protected[svaractor] def set[T](sVar: SVar[T], value: T, at : Time): Boolean =
    set(sVar, value, at, forceUpdate = false)

  private[svaractor] def set[T](sVar : SVar[T], value: T, at : Time, forceUpdate : Boolean) = sVar match {
    case convertedSVar : ConvertedSVar[_,T] =>
      convertedSVar.set(value, at, forceUpdate)
    case _ if sVar.isMutable =>
      val currentOwner = owner(sVar)
      if (currentOwner isSameAs self)
        write(self, sVar, value, at, forceUpdate)
      else
        currentOwner ! WriteSVarMessage( self, sVar, value, at, forceUpdate )
      true
    case _ => false
  }


  protected[svaractor] def notifyObserver[T](observer : SVarActor.Ref, msg : NotifyWriteSVarMessage[T]){
    observer ! msg
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
  protected[svaractor] def get[T : ClassTag ](stateVariable : StateParticle[T], at : Time = Now,
                                              accessMethod : AccessMethod = GetClosest)( consume: T => Unit )  {
    get(at, accessMethod, stateVariable)((x : ContentType[T]) => consume(x._1))
  }

  protected[svaractor] def get[T : ClassTag ](at : Time, accessMethod : AccessMethod, stateVariable : StateParticle[T])
                                             ( consume: ContentType[T] => Unit )  {
    stateVariable match {
      case convertedSVar : ConvertedSVar[_, T] =>
        convertedSVar.get(at, accessMethod)(consume)
      case sval : SValType[T] =>
        sval.get(at, accessMethod)(consume)
      case sVar : SVar[T] =>
        if( owner( sVar ) isSameAs self )
          consume ( read( sVar, at, accessMethod ) )
        else {
          owner( sVar ) ! ReadSVarMessage( sVar, at, accessMethod )
          addSingleUseHandler[ValueOfSVarMessage[T]]( {
            case ValueOfSVarMessage( svar, value) if svar == sVar => consume(value)
          } : PartialFunction[ValueOfSVarMessage[T], Unit])
        }
    }
  }

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
  protected[svaractor] def observe[T](sVar : SVar[T], ignoredWriters: Set[SVarActor.Ref] = Set())(handler: ContentType[T] => Unit) =
    sVar match {
      case convertedSVar : ConvertedSVar[_, T] =>
        convertedSVar.observe((v, t) => handler(v -> t), ignoredWriters)
      case _ =>
        if ( owner( sVar) isSameAs self )
          addObserver( sVar, self, ignoredWriters )
        else
          owner( sVar ) ! ObserveSVarMessage( sVar, self, ignoredWriters )
        addSVarObserveHandler(sVar)(handler)
    }

  /**
   *  This method returns the last known owner of the state variable.
   *
   */
  final protected[svaractor] def owner[T](sVar : SVar[T] ) : Owner =
    sVar match {
      case convertedSVar : ConvertedSVar[_,_] =>
        owner( convertedSVar.wrappedSVar )
      case mutableSVar : SVar[T] =>
        getOrUpdateSVarOwner(sVar, mutableSVar.initialOwner)
      case _ => NoOwner
    }

  private case class KnownOwner(owner : SVarActor.Ref) extends Owner(owner.tell){
    def isSameAs(that: SVarActor.Ref): Boolean = owner equals that
  }

  private case object NoOwner extends Owner( (_, _) => {} ){
    def isSameAs(x : SVarActor.Ref) = false
  }

  protected abstract class Owner( val tell : (Any, SVarActor.Ref) => Unit ){
    def !(msg: Any)(implicit sender : SVarActor.Ref){ tell(msg, sender) }
    def isSameAs(that : SVarActor.Ref) : Boolean
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

    if(currentOwner isSameAs self)
      changeOwner(self, sVar, newOwner, bufferMode = data(sVar).getBufferSetting)
    else
      currentOwner ! ChangeOwnerOfSVarMessage( sVar, newOwner, data(sVar).getBufferSetting )
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

  protected class ChangeOwnerStatus(val svar : SVar[_], val newOwner : SVarActor.Ref) {
    private def clearQueueFor( actor : SVarActor.Ref )(implicit context : SVarActor) {
      heldMessages.get( actor ) match {
        case None =>
        case Some(queue) =>
          heldMessages -= actor
          // forward messages if the current actor was the sender, return to sender otherwise
          if (actor == context)
            queue.foreach( newOwner ! _ )
          else
            actor ! HeldMessagesMessage( svar, newOwner, queue.toList)(self)
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
  private val heldMessages = mutable.WeakHashMap[SVar[_], ChangeOwnerStatus]()

  //! the locally known owners
  private val sVarOwners = mutable.WeakHashMap[SVar[_], SVarActor.Ref]()
  protected val deadOwners = mutable.Set[SVarActor.Address]()

  //!
  protected var isRunning : Boolean = true

  protected val sVarObserveHandlers = mutable.WeakHashMap[SVar[_], ContentType[Any] => Unit]()

  //
  //
  // package private methods (at least most of them)
  //
  //

  //! !!! DO NOT USE THIS FUNCTION BUT THE SVARS OWNER() FUNCTION UNLESS YOU KNOW EXACTLY WHAT YOU ARE DOING !!!
  private[svaractor] def getLocallyKnownSVarOwner(svar: SVar[_]): Option[SVarActor.Ref] =
    sVarOwners.get(svar)

  private def getOrUpdateSVarOwner(svar: SVar[_], newOwner : SVarActor.Ref) : Owner = sVarOwners.get(svar) match{
    case Some(owner) => KnownOwner(owner)
    case None =>
      if (deadOwners.contains(SVarActor.addressOf(newOwner))){
        NoOwner
      } else {
        sVarOwners.update(svar, newOwner)
        KnownOwner(newOwner)
      }
  }

  private[svaractor] def addSVarObserveHandler[T](svar : SVar[T])( handler : ContentType[T] => Unit ) : java.util.UUID = {
    sVarObserveHandlers.update(svar, handler.asInstanceOf[ContentType[Any] => Unit])
    java.util.UUID.randomUUID()
  }

  private[svaractor] def removeSVarObserveHandlers(svar : SVar[_]) =
    sVarObserveHandlers -= svar

  private[svaractor] def addSVarOwner(svar: SVar[_], owner : SVarActor.Ref) {
    sVarOwners += svar -> owner
  }

  private[svaractor] def changeOwner[T](sender     : SVarActor.Ref,
                                        svar       : SVar[T],
                                        newOwner   : SVarActor.Ref,
                                        value      : Option[(SVal[T,TypeInfo[T,T]], Time)] = None,
                                        bufferMode : BufferMode = Unbuffered ) : Boolean = {
    if (self == newOwner){
      if (!this.isOwnerOf(svar)) value match {
        case Some((_data, timeStamp)) => insertSVar(svar, _data, timeStamp, bufferMode)
        case None => throw new Exception
      }
      sender ! AcceptSVarMessage(svar)
    }
    else if (isOwnerOf(svar)) heldMessages.get(svar) match {
      case Some(changeOwnerStatus) if changeOwnerStatus.isAccepted => owner(svar, newOwner ) //!!! is this correct? don't we have to tell "ownerHasChanged"?
      case Some(changeOwnerStatus) => changeOwnerStatus.pushMessage(ChangeOwnerOfSVarMessage( svar, newOwner, bufferMode))
      case None =>
        heldMessages(svar) = new ChangeOwnerStatus(svar, newOwner)
        newOwner ! BunchOfSimXMessagesMessages(
          OfferSVarMessage( getOriginal(svar), data(svar).readFull(Now, GetClosest), bufferMode ) ::  moveObservers(svar, newOwner)
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

  protected def getSVarsObservedBy( observer : SVarActor.Ref ) =
    data.filter( pair => pair._2.isObservedBy( observer) )

  private def getObservedBy( addressEquals : SVarActor.Ref => Boolean ) =
    data.filter( d => d._1.isMutable && d._2.getObservers.keys.exists( addressEquals ) ).map{
      tuple => tuple._1.asInstanceOf[SVar[_]] -> tuple._2.getObservers.find( x => addressEquals(x._1) ).get._1
    }

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
  protected[svaractor] def createSVar[T]( value: SVal[T,TypeInfo[T,T]], timeStamp : Time, bufferMode : BufferMode ) = {
    val retVal = new SVarImpl(self, value.typedSemantics.classTag, value.typedSemantics.typeTag)
    insertSVar(retVal, value, timeStamp, bufferMode)
    retVal
  }

  private def insertSVar[T]( sVar: SVar[T], value: SValType[T], timeStamp : Time, bufferSetting : BufferMode ) {
    if (sVar.isMutable){
      val dataSVar = new SVarDataImpl( value, timeStamp, new WeakReference( sVar ), bufferSetting )
      GarbageCollectionObserver.observe(dataSVar)
      data += sVar -> dataSVar
      addSVarOwner(sVar, self)
    }
  }

  //  private def removeSVar( svar : SVar[_] ) = {
  //    removeSVarOwner(svar)
  //    removeSVarData(svar)
  //  }

  private def removeSVarData( sVar: SVar[_] ) =
    data -= sVar

  private[svaractor] final def write[T]( writer: SVarActor.Ref, sVar: SVar[T], value: T, at : Time, forceUpdate : Boolean) {
    heldMessages.get(sVar) match {
      case Some(changeOwnerStatus) => changeOwnerStatus.pushMessage( WriteSVarMessage( writer, sVar, value, at, forceUpdate) )
      case None =>
        if( forceUpdate  || !read( sVar, at, GetClosest ).equals( value )  )
          data( sVar ).write( writer, value, at )
    }
  }

  private[svaractor] def read[T]( sVar: SVar[T], at : Time, accessMethod : AccessMethod ) : ContentType[T] =
    data( sVar ).read(at, accessMethod).asInstanceOf[ContentType[T]]

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
      case None => if (data contains sVar) internalRemoveObserver(sVar, a)
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
    heldMessages.contains(svar)

  private def isOwnerOf(svar : SVar[_]) : Boolean =
    data.contains(svar)

  private def updateSVarOwner(svar : SVar[_], newOwner : SVarActor.Ref) {
    sVarOwners.update(svar, newOwner)
  }

  private def handleOwnerDependentMsg[T <: SVarHoldingMessage[_]]( handler : T => Unit )( msg : T ) {
    if ( isOwnerChangeInProgress(msg.sVar) )
      changeOwnerInProgessHandler( msg )
    else if ( isOwnerOf(msg.sVar) )
      handler( msg )
    else owner(msg.sVar) match {
      case KnownOwner(owner) => msg.sender ! SVarOwnerChangedMessage( msg.sVar, owner, msg )
      case _ =>
    }
  }

  private def createValueOfSVarMsg[T](sVar : SVar[T], at : Time, accessMethod : AccessMethod) : ValueOfSVarMessage[T] =
    ValueOfSVarMessage( sVar, read(sVar, at, accessMethod))

  //
  //
  //  public methods
  //
  //


  /**
   *
   */
  def shutdown() {
    //isRunning = false
    //TODO: implement this
    context.stop(self)
  }

  final protected def handleMessage : PartialFunction[Any, Any] =
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
    msg => changeOwner( msg.sender, msg.sVar, self, Some( msg.value ), msg.bufferMode )
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
      val successful = changeOwner(msg.sender, msg.sVar, msg.newOwner, bufferMode = msg.bufferMode)
      if (! successful) getLocallyKnownSVarOwner(msg.sVar) match {
        case Some(owner) => msg.sender ! SVarOwnerChangedMessage( msg.sVar, owner, msg)
        case _           => msg.sender ! UnknownSVarMessage(msg.sVar, msg)
      }
    }
  }

  addHandler[CreateSVarMessage[Any]] { msg =>
    msg.sender ! SVarCreatedMessage( SVarImpl( msg.value, msg.timeStamp ), msg )
  }

  addHandler[ReadSVarMessage[_]]{
    handleOwnerDependentMsg( msg => msg.sender ! createValueOfSVarMsg( msg.sVar, msg.at, msg.accessMethod ) )
  }

  addHandler[WriteSVarMessage[Any]]{
    handleOwnerDependentMsg( msg => write( msg.writer, msg.sVar, msg.value, msg.at, msg.forceUpdate ) )
  }

  addHandler[ObserveSVarMessage[_]]{
    handleOwnerDependentMsg( msg => addObserver(msg.sVar, msg.observer, msg.ignoredWriters) )
  }

  addHandler[IgnoreSVarMessage[_]]{
    handleOwnerDependentMsg( msg => removeObserver(msg.sVar, msg.observer) )
  }

  addHandler[NotifyWriteSVarMessage[_]] {
    handleNotifyWrite
  }

  private[svaractor] def handleNotifyWrite(msg : NotifyWriteSVarMessage[_]){
    sVarObserveHandlers get msg.sVar collect { case handler => handler( msg.value ) }
  }

  addHandler[SVarOwnerChangedMessage[_]] { msg =>
    addSVarOwner(msg.sVar, msg.newOwner)
    msg.newOwner ! msg.originalMessage
  }

  //multimessage related functions
  addHandler[BunchOfSimXMessagesMessages]{
    handleBunchOfMessages
  }

  addHandler[AtomicUpdate]{
    handleAtomicUpdate
  }

  addHandler[AtomicSet]{
    handleAtomicSet
  }

  private[svaractor] def handleAtomicUpdate(msg : AtomicUpdate){
    handleBunchOfMessages(BunchOfSimXMessagesMessages(msg.msgs))
  }

  private[svaractor] def handleAtomicSet(msg : AtomicSet){
    handleBunchOfMessages(BunchOfSimXMessagesMessages(msg.msgs))
  }

  private def handleBunchOfMessages(msg : BunchOfSimXMessagesMessages){
    msg.msgs.foreach( applyHandlers )
  }

  addHandler[DisassociatedEvent]{ msg =>
    def sameAsDisassociated(ref : SVarActor.Ref) = SVarActor.addressOf(ref) == msg.remoteAddress
    getObservedBy(sameAsDisassociated).foreach( tuple => removeObserver(tuple._1, tuple._2) )
    val toRemove = sVarOwners.filter{ tuple => sameAsDisassociated(tuple._2) }.keys
    deadOwners += msg.remoteAddress
    sVarOwners --= toRemove
  }

  protected def addJobIn( in : Long )( job : => Unit ) {
    addJobAt(System.currentTimeMillis() + in)(job)
  }

  private case class JobRequest(at : Long, job : () => Unit )

  addHandler[JobRequest]{
    request => addJobAt(request.at)(request.job())
  }

  def requestJobIn(in : Long)( job : => Unit) : Unit ={
    self ! JobRequest(System.currentTimeMillis() + in, () => job)
  }
}

trait SVarFunctions{
  protected[svaractor] def set[T](sVar : SVar[T], value: T, at : Time) : Boolean
  protected[svaractor] def get[T : ClassTag /*: TypeTag*/]( at : Time, accessMethod : AccessMethod, sVar : StateParticle[T])(consume: ContentType[T] => Unit)
  protected[svaractor] def observe[T](sVar : SVar[T])( handler: ContentType[T] => Unit) : java.util.UUID = observe(sVar, Set[SVarActor.Ref]())(handler)
  protected[svaractor] def observe[T](sVar : SVar[T], ignoredWriters: Set[SVarActor.Ref] = Set())(handler: ContentType[T] => Unit) : java.util.UUID
  protected[svaractor] def ignore[T](sVar : SVar[T] )
  protected[svaractor] def owner[T](sVar : SVar[T], newOwner: SVarActor.Ref)
  //  protected[svaractor] def owner[T](sVar : SVar[T] ) : SVarActor.Ref
}