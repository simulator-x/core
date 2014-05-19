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

import simx.core.svaractor.{SVarActor, SimXMessage}
import java.util.UUID
import simx.core.entity.Entity
import scala.annotation.meta.param

/**
 * This method is sent while introducing a sync group. The actor that wants to introduce a sync group sends this message
 * to the dedicated leader. The dedicated leader answers with a [[simx.core.svaractor.synclayer.SyncGroupAccepted]]
 * or a [[simx.core.svaractor.synclayer.SyncGroupDeclined]] message.
 *
 * @author Stephan Rehfeld
 *
 * @param syncGroup The sync group that should be introduced.
 */
case class OfferSyncGroup( syncGroup : SyncGroup )(implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage {
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
}

/**
 * This message is sent as an answers on a [[simx.core.svaractor.synclayer.OfferSyncGroup]] message if something went
 * wrong. Typically if the sync already has been introduced. This errors normally won't happen!
 *
 * @author Stephan Rehfeld
 *
 * @param syncGroup The sync group that was not introduced.
 */
case class SyncGroupDeclined( syncGroup : SyncGroup )(implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage {
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
}

/**
 * This message is sent as an answers on a [[simx.core.svaractor.synclayer.OfferSyncGroup]] message if the sync group
 * was successfully introduced.
 *
 * @author Stephan Rehfeld
 *
 * @param syncGroup The sync group that was not introduced.
 */
case class SyncGroupAccepted( syncGroup : SyncGroup )(implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage {
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
}

/**
 * This message is sent to the leader of a sync group if an entity should be added to the sync group. The leader replies
 * with a [[simx.core.svaractor.synclayer.AddedEntityToSyncGroup]] on success or with a
 * [[simx.core.svaractor.synclayer.EntityAlreadyInSyncGroup]] or
 * [[simx.core.svaractor.synclayer.NotTheLeaderOfTheSyncGroup]] if something failed.
 *
 * @author Stephan Rehfeld
 *
 * @param uuid The UUID of the request. Used to call the right handler function.
 * @param entity The entity to add.
 * @param syncGroup The sync group.
 */
case class AddEntityToSyncGroup( uuid : UUID, entity : Entity, syncGroup : SyncGroup )
                               (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage {
  require( uuid != null, "The parameter 'uuid' must not be 'null'!" )
  require( entity != null, "The parameter 'entity' must not be 'null'!" )
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
}

/**
 * This message is sent as a reply on a [[simx.core.svaractor.synclayer.AddEntityToSyncGroup]] if the entity
 * was successfully added.
 *
 * @author Stephan Rehfeld
 *
 * @param uuid The UUID of the original add request.
 * @param entity The entity that was added.
 * @param syncGroup The sync group.
 */
case class AddedEntityToSyncGroup( uuid : UUID, entity : Entity, syncGroup : SyncGroup )
                                 (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage {
  require( uuid != null, "The parameter 'uuid' must not be 'null'!" )
  require( entity != null, "The parameter 'entity' must not be 'null'!" )
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
}

/**
 * This message is sent as a reply on a [[simx.core.svaractor.synclayer.AddEntityToSyncGroup]] if the entity
 * was already in the sync group.
 *
 * @author Stephan Rehfeld
 *
 * @param uuid The UUID of the original add request.
 * @param entity The entity that was already in the sync group.
 * @param syncGroup The entity that was added.
 */
case class EntityAlreadyInSyncGroup( uuid : UUID, entity : Entity, syncGroup : SyncGroup )
                                   (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage {
  require( uuid != null, "The parameter 'uuid' must not be 'null'!" )
  require( entity != null, "The parameter 'entity' must not be 'null'!" )
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
}

/**
 * This message is sent as a reply on a [[simx.core.svaractor.synclayer.AddEntityToSyncGroup]] or a
 * [[simx.core.svaractor.synclayer.SynchronizeOnSyncGroup]] message if the target actor is not the leader of the sync
 * group.
 *
 * This error normally not occurs and indicates a serious problem.
 *
 * @author Stephan Rehfeld
 *
 * @param uuid Maybe the UUID of the original add request.
 * @param syncGroup The entity that was added.
 */
case class NotTheLeaderOfTheSyncGroup( uuid : UUID, syncGroup : SyncGroup )
                                     (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage {
  require( uuid != null, "The parameter 'uuid' must not be 'null'!" )
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
}


/**
 * This message is sent to a sync group leader from an actor that wants to synchronize on a sync group.
 * This actor gets [[simx.core.svaractor.synclayer.WorldStepComplete]] when a new world step is complete.
 *
 * As a reply the leader can sent a [[simx.core.svaractor.synclayer.YouAlreadySynchronizedOn]] or
 * [[simx.core.svaractor.synclayer.NotTheLeaderOfTheSyncGroup]] message if something failed. The last error should not
 * happen and indicates a serious error.
 *
 * If the everything works fine the leader sends a [[simx.core.svaractor.synclayer.EntitiesOfSyncGroup]] message.
 *
 * @author Stephan Rehfeld
 *
 * @param syncGroup The sync group.
 */
case class SynchronizeOnSyncGroup( syncGroup : SyncGroup )
                                 (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage {
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
}

/**
 * This message is sent as a reply on a [[simx.core.svaractor.synclayer.SynchronizeOnSyncGroup]] if the actor is already
 * synchronized with this sync group.
 *
 * @author Stephan Rehfeld
 *
 * @param syncGroup The sync group.
 */
case class YouAlreadySynchronizedOn( syncGroup : SyncGroup )
                                   (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage {
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
}

/**
 * This message is sent after an actor newly synchronized with a sync group or if an entity was added to the sync group.
 *
 * @author Stephan Rehfeld
 *
 * @param entities A set of all current entities of the sync group.
 * @param syncGroup  The sync group.
 */
case class EntitiesOfSyncGroup( entities : Set[Entity], syncGroup : SyncGroup )
                              (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage  {
  require( entities != null, "The parameter 'entities' must not be 'null'!" )
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
}

/**
 * This message is sent by the leader of the sync group to all synchronized actors when a new world step is completed.
 *
 * @author Stephan Rehfeld
 *
 * @param syncGroup The sync group.
 */
case class WorldStepComplete( syncGroup : SyncGroup )
                            (implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage {
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
}
