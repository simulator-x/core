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

import simx.core.svaractor.TimedRingBuffer.{ContentType, Time}
import simx.core.svaractor._
import scala.reflect.ClassTag

/**
 * This trait offers some functions the sync the actor on a [[simx.core.svaractor.synclayer.SyncGroup]].
 *
 * @author Stephan Rehfeld
 */
trait SyncGroupHandling extends SVarActor {

  /**
   * The [[simx.core.svaractor.synclayer.Cache]]s of the sync groups.
   */
  private var caches = Map[SyncGroup,Cache]()

  /**
   * The handler functions that are called when a new world step is available.
   */
  private var observeErrorHandler = Map[SyncGroup,((Any) => Unit)]()

  /**
   * A small private error handler that prints the error and quits the application with error code -202.
   *
   * @param e The error.
   */
  private def defaultErrorHandler( e : Any ) {
    println( e )
    System.exit( -202 )
  }

  /**
   * This method synchronize this actor on the the given sync group.
   *
   * @param syncGroup The sync group.
   * @param onWorldStepComplete A function that is called when a new world state is available.
   * @param err An error handler that if the synchronization failed.
   */
  def syncOn( syncGroup : SyncGroup, onWorldStepComplete : (SyncGroup) => Unit, err : (Any) => Unit = defaultErrorHandler ) {
    require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
    require( onWorldStepComplete != null, "The parameter 'onWorldStepComplete' must not be 'null'!" )
    require( err != null, "The parameter 'err' must not be 'null'!" )

    syncGroup.leader ! SynchronizeOnSyncGroup( syncGroup )
    caches = caches + (syncGroup -> new Cache(syncGroup,onWorldStepComplete))
    observeErrorHandler = observeErrorHandler + (syncGroup -> err)
  }

  addHandler[YouAlreadySynchronizedOn] {
    msg =>
      val handler = observeErrorHandler( msg.syncGroup )
      observeErrorHandler = observeErrorHandler - msg.syncGroup
      handler( msg )
  }

  addHandler[NotTheLeaderOfTheSyncGroup] {
    msg =>
      val handler = observeErrorHandler( msg.syncGroup )
      observeErrorHandler = observeErrorHandler - msg.syncGroup
      handler( msg )
  }

  addHandler[EntitiesOfSyncGroup] {
    msg =>
      val observeHandler = Map[SVar[_],(Any)=>Unit]()
      for( entity <- msg.entities ) {
        for( sVarDescription <- msg.syncGroup.sVarDescriptions ) {
//          entity.get( sVarDescription ).headOption.collect{
//            case sVar : SVar[_] =>
//              caches( msg.syncGroup ).add( entity )
//              if( sVarObserveHandlers.contains( sVar ) ) {
//                observeHandler = observeHandler + (sVar -> sVarObserveHandlers( sVar ) )
//                sVarObserveHandlers.update( sVar, caches( msg.syncGroup ).updateData( sVar ) )
//              } else {
//                super.observe( sVar )( caches( msg.syncGroup ).updateData( sVar ) )
//              }
//          }
        }
      }
      for( (sVar,handler) <- observeHandler ) caches(msg.syncGroup).addUpdateFunction( sVar, handler )
  }


  /**
   * A overwritten get function that is reading the values from the cache.
   */
  override protected[svaractor] def get[T : ClassTag /*: TypeTag*/](at : Time, accessMethod : AccessMethod, sVar : StateParticle[T])( consume: ContentType[T] => Unit ) {
    val cache = caches.foldLeft[Option[Cache]]( None )( (last : Option[Cache], d : (SyncGroup,Cache) ) => if( last.isDefined ) last else { if( d._2.hasDataFor( sVar ) ) Some( d._2 ) else None} )
    cache match {
      case Some( c ) =>
        val data = c.getDataFor( sVar )
        consume( data )
      case None =>
        super.get(at, accessMethod, sVar)(consume)
    }

  }

  /**
   * A overwritten observe function that is handling the observe function correctly.
   */
  override protected[svaractor] def observe[T](sVar : SVar[T], ignoredWriters: Set[SVarActor.Ref] )(handler: ContentType[T] => Unit) = {
    val cache = caches.foldLeft[Option[Cache]]( None )( (last : Option[Cache], d : (SyncGroup,Cache) ) => if( last.isDefined ) last else { if( d._2.doesCacheSVar( sVar ) ) Some( d._2 ) else None} )
    cache match {
      case Some( c ) =>
        val retVal = super.observe( sVar, ignoredWriters )( tuple => handler(tuple ) )
        sVarObserveHandlers.update( sVar, c.updateData( sVar ) ) //TODO
        c.addUpdateFunction( sVar, handler.asInstanceOf[(Any => Unit)] )
        retVal
      case None =>
        super.observe( sVar, ignoredWriters )( tuple => handler(tuple) )
    }
  }

  /**
   * A overwritten ignore function.
   */
  override protected[svaractor] def ignore[T](sVar : SVar[T] ) {
    val cache = caches.foldLeft[Option[Cache]]( None )( (last : Option[Cache], d : (SyncGroup,Cache) ) => if( last.isDefined ) last else { if( d._2.doesCacheSVar( sVar ) ) Some( d._2 ) else None} )
    cache match {
      case Some( c ) =>
        c.removeUpdateFunction( sVar )
      case None =>
        super.ignore( sVar )
    }
  }

  addHandler[WorldStepComplete] {
    msg =>
      caches( msg.syncGroup ).worldStepComplete()
  }

  /**
   * This method returns if any cache provides a new world step.
   *
   * @return Returns 'true' is any sync group is actor in synchronized on provides a new world state.
   */
  def newWorldStepAvailable() = caches.foldLeft( true )( (prev,x) => prev || x._2.isWorldNextWorldStepAvailable )

  /**
   * This method returns if the given sync group provides a new world state.
   *
   * @param syncGroup The sync group.
   * @return Returns 'true' is a new world is available for this sync group.
   */
  def newWorldStepAvailable( syncGroup : SyncGroup ) = caches( syncGroup ).isWorldNextWorldStepAvailable

  /**
   * This method steps forward a sync groups.
   *
   * @param syncGroup The sync group to step forward.
   */
  def stepForward( syncGroup : SyncGroup ) {
    require( syncGroup != null, "The parameter 'syncGroup' must not be 'null'!" )
    caches( syncGroup ).update()
  }

  /**
   * This method steps forward all sync groups.
   */
  def stepForward() {
    for( c <- caches ) c._2.update()
  }

}
