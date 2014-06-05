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

import simx.core.svaractor.{SVarActor, SVar}
import simx.core.entity.Entity

/**
 * This class implements a three level cache for the sync groups.
 *
 * The first level saves the current active world state. A new world state is collected in the second level cache.
 * If a complete world step is saved in the second level cache, a next world step is collected in the third level cache.
 * If the third an second level caches contain a complete world step, the new values in the third level cache are
 * "lifted" to the second level cache and a new world step is collected in the third level cache.
 *
 * Every time a new world state is available the cache calls a handler function.
 *
 * The method update lift the current second level cache to the first level cache. The observe function of the sVars
 * are called at this step. The values in the current third level cache are lifted to the second level cache and the
 * third level cache is cleaned.
 *
 * @author Stephan Rehfeld
 *
 * @param syncGroup The [[simx.core.svaractor.synclayer.SyncGroup]] of this cache.
 * @param onWorldStepComplete The function that is called, if a new world step is available.
 * @param actorContext The actor context (filled out by the compiler/runtime)
 */
private[synclayer] class Cache( val syncGroup : SyncGroup, onWorldStepComplete : (SyncGroup) => Unit )( implicit actorContext : SVarActor ) {
  require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
  require( onWorldStepComplete != null, "The parameter 'onWorldStepComplete' must not be 'null'!" )
  require( actorContext != null, "The parameter 'actorContext' must not be 'null'!" )

  /**
   * The first level cache.
   */
  private var firstLevel = Map[SVar[_],Option[_]]()

  /**
   * The second level cache.
   */
  private var secondLevel = Map[SVar[_],Option[_]]()

  /**
   * The third level cache.
   */
  private var thirdLevel = Map[SVar[_],Option[_]]()

  /**
   * The observe functions of the state variables.
   */
  private var updateFunctions = Map[SVar[_],(Any => Unit)]()

  /**
   * A flag, if the second level cache contains a complete world step.
   */
  private var secondLevelCacheIsSealed = false

  /**
   * This method adds an entity to the cache. Memory is allocated for the variables that are synced by the
   * [[simx.core.svaractor.synclayer.SyncGroup]]. The current values are read.
   *
   * @param e The entity that should be added to the cache.
   */
  def add( e : Entity ) {
    require( e != null, "The parameter 'e' must not be 'null'!" )

    for( sVarDescription <- syncGroup.sVarDescriptions ) {
      val sVar = e.get(sVarDescription).head
      firstLevel = firstLevel + ( sVar -> None )
      secondLevel = secondLevel + (sVar -> None )
      thirdLevel = thirdLevel + (sVar -> None )

      sVar.get( (x) => {
        firstLevel = firstLevel + ( sVar -> Some(x) )
        secondLevel = secondLevel + (sVar -> Some(x) )
      })
    }
  }

  /**
   * This function is called to signal the cache that a world step is completed. Regarding to the current state the
   * second level cache is sealed or updated by values of the third level cache. New values are saved in the third level
   * cache.
   */
  def worldStepComplete() {
    if( secondLevelCacheIsSealed ) {
      for( (sVar,data) <- secondLevel ) {
        if( thirdLevel( sVar ).isDefined ) {
          secondLevel = secondLevel + (sVar -> thirdLevel( sVar ) )
        }
        thirdLevel = thirdLevel + (sVar -> None)
      }
    } else {
      secondLevelCacheIsSealed = true
    }
    this.onWorldStepComplete( syncGroup )
  }

  /**
   * This method updates the cache. Are values of the second level cache are written into the first level cache.
   * Registered observe functions are called. New values are saved in the second level cache, the third level cache
   * is cleaned.
   */
  def update() {
    if( secondLevelCacheIsSealed ) {
      var handlerToExecute = Map[SVar[_],(Any=>Unit)]()
      for( (sVar,data) <- secondLevel ) {
        if( data.isDefined ) {
          firstLevel = firstLevel + (sVar -> data)
          if( updateFunctions contains( sVar ) ) handlerToExecute = handlerToExecute + (sVar ->updateFunctions(sVar) )
        }
        if( thirdLevel( sVar ).isDefined ) {
          secondLevel = secondLevel + (sVar -> thirdLevel( sVar ) )
        }
        thirdLevel = thirdLevel + (sVar -> None)
      }
      for( (sVar,function) <- handlerToExecute ) function( firstLevel( sVar ).get )
      secondLevelCacheIsSealed = false
    }
  }

  /**
   * This method returns if the cache, caches values for the given state variable.
   *
   * @param sVar The state variable.
   * @return Returns 'true', if the cache caches data for this state variable.
   */
  def doesCacheSVar( sVar : SVar[_] ) = firstLevel.contains( sVar )

  /**
   * Return if the cache has any data for this sVar.
   *
   * @param sVar The state variable.
   * @return Returns 'true', if the cache holds any data for the state variable.
   */
  def hasDataFor( sVar : SVar[_] ) = doesCacheSVar( sVar ) && firstLevel( sVar ).isDefined

  /**
   * This method returns the value of the state variable that is saved in the first level cache.
   *
   * @param sVar The state variable.
   * @tparam T The state type of the encapsulated data.
   * @return The value of the state variable read from the cache.
   */
  def getDataFor[T]( sVar : SVar[T] ) : T = firstLevel( sVar ).get.asInstanceOf[T]

  /**
   * This method sets the observe function of a state variable. This function is called on an update of the cache.
   *
   * @param sVar The state variable.
   * @param f The update function.
   */
  def addUpdateFunction( sVar : SVar[_], f : (Any => Unit) ) {
    require( sVar != null, "The parameter 'sVar' must not be 'null'!" )
    require( f != null, "The parameter 'f' must not be 'null'!" )
    updateFunctions = updateFunctions + (sVar -> f)
  }

  /**
   * This method removes a observe function from state variable.
   *
   * @param sVar The state variable.
   */
  def removeUpdateFunction( sVar : SVar[_] ) {
    require( sVar != null, "The parameter 'sVar' must not be 'null'!" )
    updateFunctions = updateFunctions - sVar
  }

  /**
   * This method updates the cached data of a state variable.
   *
   * @param sVar The state variable.
   * @param v The variable
   * @return Nothing
   */
  def updateData( sVar : SVar[_] )( v : Any ) {
    if( !secondLevelCacheIsSealed ) {
      secondLevel = secondLevel + (sVar -> Some( v ) )
    } else {
      thirdLevel = thirdLevel + ( sVar -> Some( v ) )
    }
  }

  /**
   * This method returns if a new world step is available.
   *
   * @return Returns 'true' if a new world step is available.
   */
  def isWorldNextWorldStepAvailable = this.secondLevelCacheIsSealed

  // CURRENTLY NOT NEEDED, COMMENTED OUT TO PREVENT DEAD CODE
  /*def remove( e : Entity ) {
   for( sVarDescription <- syncGroup.sVarDescriptions ) {
     val sVar = e.get(sVarDescription).get
     firstLevel = firstLevel - sVar
     secondLevel = secondLevel - sVar
     thirdLevel = thirdLevel - sVar

     sVar.ignore()
   }
 } */

}
