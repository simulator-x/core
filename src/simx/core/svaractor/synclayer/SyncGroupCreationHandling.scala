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

package simx.core.svaractor.synclayer

import simx.core.entity.Entity
import java.util.UUID
import simx.core.svaractor.SVarActor

/**
 * This trait is used to create a sync group an to add entities to it. A new sync group is created by calling the
 * introduce function. After the sync group is introduced an the [[simx.core.svaractor.synclayer.SyncGroup]] object
 * is available, entites can be added to the sync group using the add function.
 *
 * @author Stephan Rehfeld
 */
trait SyncGroupCreationHandling extends SVarActor {

  /**
   * A map that holds the handler function, after the sync group is fully created.
   */
  private var introduceHandlerFunctions = Map[SyncGroup,((SyncGroup)=>Unit)]()

  /**
   * A map that holds the handler functions that are called after an entity has been added to the sync group.
   */
  private var addHandlerFunctions = Map[UUID,((Entity,SyncGroup) => Unit)]()

  /**
   * A map that hold the error handler functions that are called after adding an entity to a sync group failed.
   */
  private var addErrorFunctions = Map[UUID,(Any => Unit)]()

  /**
   * A small default error handler that prints the error object to the stream an quites the application with code -201.
   *
   * @param e The error.
   */
  private def defaultErrorHandler( e : Any ) {
    println( e )
    System.exit( -201 )
  }

  /**
   * This function introduces a new sync group. It takes the [[simx.core.svaractor.synclayer.SyncGroupDescription]] as
   * parameter and contacts the dedicated leader. After the leader accepted the sync group the handler function is
   * called and the actual [[simx.core.svaractor.synclayer.SyncGroup]] is provided.
   *
   * @param syncGroupDescription The sync group description.
   * @param h The handler function that is called after the sync group is created and accepted by the leader..
   * @return None
   */
  def introduce( syncGroupDescription : SyncGroupDescription )( h : (SyncGroup) => Unit ) {
    require( syncGroupDescription != null, "The parameter 'syncGroupDescription' must not be 'null'!" )
    require( syncGroupDescription.sVarDescriptions.size > 0, "The sync group must at least sync one type of state variable!" )
    require( h != null, "The parameter 'h' must not be 'null'!" )

    val syncGroup = SyncGroup( syncGroupDescription.leader, syncGroupDescription.sVarDescriptions, UUID.randomUUID() )
    syncGroup.leader ! OfferSyncGroup( syncGroup )
    introduceHandlerFunctions = introduceHandlerFunctions + (syncGroup -> h)
  }

  addHandler[SyncGroupAccepted] {
    msg =>
      val handler = introduceHandlerFunctions(msg.syncGroup)
      introduceHandlerFunctions = introduceHandlerFunctions - msg.syncGroup
      handler( msg.syncGroup )
  }

  addHandler[SyncGroupDeclined] {
    msg =>
      println( "DECLINED SYNCGROUP: " + msg.syncGroup )
      println( "LEADER: " + msg.sender )
      throw new IllegalStateException("A SyncGroup has been decline, this should not happen" )

  }

  /**
   * This method adds an entity to a [[simx.core.svaractor.synclayer.SyncGroup]]. The given handler function is called
   * after the leader has confirmed, that the entity is now part of the sync group.
   *
   * @param e The entity that should be added to a sync group.
   * @param syncGroup The sync group.
   * @param h The handler function that is called after the entity has been added tot the sync group.
   * @param err The error function that is called if adding the sync group failed.
   * @return None
   */
  def add( e : Entity, syncGroup : SyncGroup )( h : ((Entity,SyncGroup) => Unit) )( err : (Any) => Unit = defaultErrorHandler ) {
    require( e != null, "The parameter 'e' must not be 'null'!" )
    require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
    require( h != null, "The parameter 'h' must not be 'null'!" )
    require( err != null, "The parameter 'err' must not be 'null'!" )
    val uuid = UUID.randomUUID()
    syncGroup.leader ! AddEntityToSyncGroup( uuid, e, syncGroup)
    addHandlerFunctions = addHandlerFunctions + (uuid -> h )
    addErrorFunctions = addErrorFunctions + (uuid -> err )
  }

  addHandler[AddedEntityToSyncGroup] {
    msg =>
      val handler = addHandlerFunctions( msg.uuid )
      addHandlerFunctions = addHandlerFunctions - ( msg.uuid )
      addErrorFunctions = addErrorFunctions - ( msg.uuid )
      handler( msg.entity, msg.syncGroup )
  }

  addHandler[EntityAlreadyInSyncGroup] {
    msg =>
      val handler = addErrorFunctions( msg.uuid )
      addHandlerFunctions = addHandlerFunctions - ( msg.uuid )
      addErrorFunctions = addErrorFunctions - ( msg.uuid )
      handler( msg )
  }

  addHandler[NotTheLeaderOfTheSyncGroup] {
    msg =>
      val handler = addErrorFunctions( msg.uuid )
      addHandlerFunctions = addHandlerFunctions - ( msg.uuid )
      addErrorFunctions = addErrorFunctions - ( msg.uuid )
      handler( msg )
  }


}
