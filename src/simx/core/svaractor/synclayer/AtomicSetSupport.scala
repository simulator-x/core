/*
 * Copyright 2014 The SIRIS Project
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

package simx.core.svaractor.synclayer

import simx.core.entity.typeconversion.ConvertedSVar
import simx.core.svaractor.TimedRingBuffer.Time
import simx.core.svaractor._

/**
 * Created by dwiebusch on 09.05.14
 */
trait AtomicSetSupport extends SVarActor{
  /**
   * methods to be used in the actor
   */

  /**
   * create atomic set block
   * @param func the method containing all calls to svar.set which shall be executed simultaneously
   */
  protected def atomicSet(func : => Any){
    beginAtomicSet()
    func
    endAtomicSet()
  }

  /**
   * register method to be executed after all messages used in atomic set have been processed
   * @param toExecute the method to be executed
   */
  protected def doAfterAtomicSet(toExecute : => Unit){
    if (atomicUpdateActive.isDefined)
      toDoAfterAtomicSet = (() => toExecute) :: toDoAfterAtomicSet
    else
      toExecute
  }

  /**
   * Atomic Update (observer side) section
   */
  private var atomicUpdateActive : Option[java.util.UUID] = None
  private var svarsInUpdate = Map[java.util.UUID, Set[SVar[_]]]()
  private var toDoAfterAtomicSet : List[() => Unit] = Nil
  private var heldMessages : List[Any] = Nil

  override private[svaractor] def handleAtomicUpdate(msg: AtomicUpdate){
    // initialize update
    if (!svarsInUpdate.contains(msg.updateId)) {
      svarsInUpdate = svarsInUpdate.updated(msg.updateId, msg.svarsInUpdate.filter(sVarObserveHandlers.keySet.contains))
      atomicUpdateActive = Some(msg.updateId)
    }
    // handle messages manually
    msg.msgs.foreach(handleNotifyWrite)
    // remove updated svars
    svarsInUpdate = svarsInUpdate.updated(msg.updateId, svarsInUpdate(msg.updateId) -- msg.msgs.map(_.sVar))
    // if all svars were updated, finish atomic update
    if (svarsInUpdate(msg.updateId).isEmpty) {
      svarsInUpdate = svarsInUpdate - msg.updateId
      atomicUpdateActive = None
      finalizeAtomicUpdate()
    }
  }

  /**
   * holds the given message to be processed later
   * @param msg the message to be held
   */
  private def holdBack(msg : Any){
    heldMessages = msg :: heldMessages
  }

  /**
   * finalizes the current atomic update
   */
  private def finalizeAtomicUpdate(){
    toDoAfterAtomicSet.reverse.foreach(_.apply())
    heldMessages.reverse.map(handleMessage)
    toDoAfterAtomicSet = Nil
    heldMessages = Nil
  }

  /**
   * injection of check before messages are handeled normally
   */
  override protected def applyHandlers(msg: Any): Any =
    if (atomicUpdateActive.isEmpty)
      super.applyHandlers(msg)
    else msg match {
      case msg : AtomicUpdate if msg.updateId == atomicUpdateActive.get => handleAtomicUpdate(msg)
      case _ => holdBack(msg)
    }

  /**
   * overridden methods
   */
  override protected[svaractor] def set[T](sVar : SVar[T], value: T, at : Time, forceUpdate : Boolean) = sVar match {
    case convertedSVar : ConvertedSVar[_,T] =>
      convertedSVar.set(value, at, forceUpdate)
    case _ if sVar.isMutable =>
      val currentOwner = owner(sVar)
      if (currentOwner isSameAs self)
        write(self, sVar, value, at, forceUpdate) // notify write (see below) will take care in this case
      else if (inAtomicSetMode) // handle foreign owner
        addAtomicSetMessage(currentOwner, WriteSVarMessage( self, sVar, value, at, forceUpdate ))
      else // standard case
        currentOwner ! WriteSVarMessage( self, sVar, value, at, forceUpdate )
      true
    case _ => false
  }

  override protected[svaractor] def notifyObserver[T](observer : SVarActor.Ref, msg : NotifyWriteSVarMessage[T]){
    if (inAtomicSetMode)
      addAtomicNotifyObserver(observer, msg)
    else
      super.notifyObserver(observer, msg)
  }

  /**
   * Atomic Set (owner side) section
   */
  private var atomicSetMode = List[java.util.UUID]()
  // the sets of foreign owners to be notified
  private var atomicOwnerNotifications = Map[java.util.UUID, Map[Owner, List[WriteSVarMessage[_]]]]()
  // the sets of observers to be notified
  private var atomicObserverNotifications = Map[java.util.UUID, Map[SVarActor.Ref, List[NotifyWriteSVarMessage[_]]]]()

  /**
   * checks if in atomic set mode
   * @return true, if in atomic set mode, otherwise false
   */
  private def inAtomicSetMode : Boolean =
    atomicSetMode.nonEmpty

  /**
   * handle for atomic set messages
   * @param msg the AtomicSet message to be handled
   */
  override private[svaractor] def handleAtomicSet(msg : AtomicSet){
    beginAtomicSet(Some(java.util.UUID.randomUUID()))
    atomicSet{ msg.msgs.foreach( applyHandlers ) }
    endAtomicSet()
  }

  /**
   * (re-)initializes an atomic set mode
   */
  private def beginAtomicSet(id : Option[java.util.UUID] = None){
    atomicSetMode = id.getOrElse(atomicSetMode.headOption.getOrElse(java.util.UUID.randomUUID())) :: atomicSetMode
  }

  /**
   * stores a foreign owner that has to be notified during the current atomic set
   * @param receiver the owner
   * @param msg the message to be sent
   */
  private def addAtomicSetMessage(receiver : Owner, msg : WriteSVarMessage[_]){
    val toUpdate = atomicOwnerNotifications.getOrElse(atomicSetMode.head, Map())
    val updated = toUpdate.updated(receiver, msg :: toUpdate.getOrElse(receiver, Nil))
    atomicOwnerNotifications = atomicOwnerNotifications.updated(atomicSetMode.head, updated)
  }

  /**
   * stores an observer that has to be notified about a value change
   * @param receiver the observer
   * @param msg the value change message
   */
  private def addAtomicNotifyObserver(receiver : SVarActor.Ref, msg : NotifyWriteSVarMessage[_]){
    val toUpdate = atomicObserverNotifications.getOrElse(atomicSetMode.head, Map())
    val updated = toUpdate.updated(receiver, msg :: toUpdate.getOrElse(receiver, Nil))
    atomicObserverNotifications = atomicObserverNotifications.updated(atomicSetMode.head, updated)
  }

  /**
   * takes care of all messages to be sent to finish the currently active atomic set process
   */
  private def endAtomicSet(){
    atomicSetMode match {
      case head :: tail =>
        atomicSetMode = tail
        //extract information about current set
        val owners = atomicOwnerNotifications.getOrElse(head, Nil)
        val observers = atomicObserverNotifications.getOrElse(head, Nil)
        atomicOwnerNotifications = atomicOwnerNotifications - head
        atomicObserverNotifications = atomicObserverNotifications - head
        val svarsInCurrentAtomicSet = owners.flatMap(_._2.map(_.sVar)).toSet ++ observers.flatMap(_._2.map(_.sVar))
        // inform observers and owners
        owners.foreach{ tuple => tuple._1 ! AtomicSet(head, tuple._2.reverse, svarsInCurrentAtomicSet) }
        observers.foreach{ tuple => tuple._1 ! AtomicUpdate(head, tuple._2.reverse, svarsInCurrentAtomicSet) }
      case Nil => // should not happen, ignore
    }
  }
}
