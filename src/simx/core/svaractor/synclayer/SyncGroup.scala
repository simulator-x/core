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

import simx.core.ontology.SVarDescription
import java.util.UUID
import java.io.{ObjectInputStream, ObjectOutputStream}
import simx.core.svaractor.SVarActor

/**
 * A sync group is used to synchronize state variables of different type along different entities to the world step,
 * that is produced by one actor, the leader.
 *
 * @author Stephan Rehfeld
 *
 * @param leader The leader of the sync group.
 * @param sVarDescriptions A set of sVar descriptions of state variables that are synced by this sync group.
 * @param uuid The UUID of the sync group.
 */
case class SyncGroup private[synclayer]( leader : SVarActor.Ref, sVarDescriptions : Set[SVarDescription[_,_] ], uuid : UUID ) {
  override def equals( o : Any ) = {
    o match {
      case s : SyncGroup => s.uuid == uuid
      case _ => false
    }
  }

  override def hashCode() = uuid.hashCode()

}

/**
 * The companion object to create a [[simx.core.svaractor.synclayer.SyncGroupDescription]].
 *
 * @author Stephan Rehfeld
 */
object SyncGroup {

  /**
   * This method creates a new [[simx.core.svaractor.synclayer.SyncGroupDescription]] for a sync group with the given
   * actor as leader.
   *
   * @param leader The actor that should become the leader of the sync group.
   * @return A sync group description.
   */
  def withLeader( leader : SVarActor.Ref ) = new SyncGroupDescription( leader, Set() )
}

/**
 * A description of a sync group that can used to create a sync group using the introduce function of the
 * [[simx.core.svaractor.synclayer.SyncGroupCreationHandling]] trait.
 *
 * @author Stephan Rehfeld
 *
 * @param leader The leader of the sync group.
 * @param sVarDescriptions A set of description for state variables that are synced by this sync group.
 */
case class SyncGroupDescription private[synclayer]( leader : SVarActor.Ref, sVarDescriptions : Set[SVarDescription[_,_] ] ) {
  require( leader != null, "The parameter 'leader' must not be 'null'!" )
  require( sVarDescriptions != null, "The parameter 'sVarDescriptions' must not be 'null'!" )

  /**
   * This method adds a state variable description to the sync group.
   *
   * @param sVarDescription The state variable description.
   * @return The altered sync group description.
   */
  def onSymbols( sVarDescription : SVarDescription[_,_] ) = SyncGroupDescription( leader, Set() + sVarDescription )

  /**
   * This method adds a state variable description to the sync group.
   *
   * @param sVarDescription The state variable description.
   * @return The altered sync group description.
   */
  def and( sVarDescription : SVarDescription[_,_] ) = SyncGroupDescription( leader, sVarDescriptions + sVarDescription )

}
