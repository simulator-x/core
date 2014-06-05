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

import simx.core.svaractor.handlersupport.HandlerSupport
import simx.core.entity.Entity
import java.util.UUID
import simx.core.svaractor.SVarActor


/**
 * This trait provides the functionality to be a leader of a sync group. The messages regarding to the introduction of
 * a sync group and the adding of entities are processes automatically. The function worldStepComplete signals all
 * synced actors that a new world step is available.
 *
 * @author Stephan Rehfeld
 */
trait SyncGroupLeader extends HandlerSupport with SVarActor{

  /**
   * The sync groups that are controlled by this actor.
   */
  private var syncGroups = Set[SyncGroup]()

  /**
   * The entities of each controlled sync group.
   */
  private var syncGroupsToEntities = Map[SyncGroup,Set[Entity]]()

  /**
   * The synchronized actors for each sync group.
   */
  private var syncGroupsToSyncedActors = Map[SyncGroup,Set[SVarActor.Ref]]()

  /**
   * This message is used the signalize, that a world step is complete. All synchronized actors will receive
   * a notification about this event.
   */
  protected def worldStepComplete() {
    for( (syncGroup,syncedActor) <- syncGroupsToSyncedActors ) for( a <- syncedActor ) a ! WorldStepComplete( syncGroup )
  }

  addHandler[OfferSyncGroup] {
    msg =>
      if( syncGroups contains msg.syncGroup ) {
        msg.sender ! SyncGroupDeclined( msg.syncGroup )
      } else {
        syncGroups = syncGroups + msg.syncGroup
        syncGroupsToEntities = syncGroupsToEntities + (msg.syncGroup -> Set())
        syncGroupsToSyncedActors = syncGroupsToSyncedActors + (msg.syncGroup -> Set())
        msg.sender ! SyncGroupAccepted( msg.syncGroup )
      }
  }

  addHandler[AddEntityToSyncGroup] {
    msg =>
      if( syncGroups contains msg.syncGroup  ) {
        if( syncGroupsToEntities( msg.syncGroup ) contains msg.entity ) {
          msg.sender ! EntityAlreadyInSyncGroup( msg.uuid, msg.entity, msg.syncGroup )
        } else {
          syncGroupsToEntities = syncGroupsToEntities + (msg.syncGroup -> (syncGroupsToEntities(msg.syncGroup) + msg.entity))
          for( syncedActor <- syncGroupsToSyncedActors( msg.syncGroup ) ) syncedActor ! EntitiesOfSyncGroup( Set() + msg.entity, msg.syncGroup )
          msg.sender ! AddedEntityToSyncGroup( msg.uuid, msg.entity, msg.syncGroup )
        }
      } else {
        msg.sender ! NotTheLeaderOfTheSyncGroup( msg.uuid, msg.syncGroup )
      }
  }

  addHandler[SynchronizeOnSyncGroup] {
    msg =>
      if( syncGroups contains( msg.syncGroup ) ) {
        if( syncGroupsToSyncedActors( msg.syncGroup ) contains msg.sender ) {
          msg.sender ! YouAlreadySynchronizedOn( msg.syncGroup )
        } else {
          syncGroupsToSyncedActors = syncGroupsToSyncedActors + (msg.syncGroup -> (syncGroupsToSyncedActors( msg.syncGroup ) + msg.sender ))
          msg.sender ! EntitiesOfSyncGroup( syncGroupsToEntities( msg.syncGroup ), msg.syncGroup )
        }
      } else {
        msg.sender ! NotTheLeaderOfTheSyncGroup( UUID.randomUUID(), msg.syncGroup )
      }
  }

}
