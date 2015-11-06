/*
 * Copyright 2015 The SIRIS Project
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

package simx.core.worldinterface.entity

import simx.core.entity.Entity
import simx.core.ontology._
import simx.core.worldinterface._
import simx.core.worldinterface.base.WorldInterfaceHandlingBase

/**
 * Created by martin 
 * on 21/04/15.
 */
private[worldinterface] trait EntityRegistrationHandling extends WorldInterfaceHandlingBase {

  private type CreationMessageHandler = (Entity) => Any
  type Path = List[Symbol]
  private var creationMessageHandlers = Map[Path, CreationMessageHandler]()

  override def preStart(): Unit = {
    super.preStart()
    addHandler[CreationMessage]{msg =>
      creationMessageHandlers.filter(h => msg.path.startsWith(h._1)).foreach(_._2.apply(msg.e))
    }
  }

  //TODO Move this method to the new entity registration 
  final protected def registerEntity(e : Entity) {
    registerEntity(Symbol(e.getSimpleName) :: Nil, e)
  }
  
  /**
   * Registers an entity using the given name as path.
   */
  final protected def registerEntity(name : Symbol, e : Entity) {
    registerEntity(name :: Nil, e)
  }

  /**
   * Registers an entity below the given path.
   */
  final protected  def registerEntity(path : Path, e : Entity) {
    WorldInterfaceActor ! EntityRegisterRequest(path, e)
  }

  final protected def unregisterEntity(e : Entity) {
    WorldInterfaceActor ! EntityUnregisterRequest(e)
  }

  /**
   *  Applies the passed handler for the next entity that will be registered below the given path.
   */
  final protected def onNextEntityRegistration(path : Path)(f : Entity => Any) {
    nonBlockingHandler[Entity](OnNextRegistration(path), f)
  }

  /**
   *  Applies the passed handler for all entities that will be registered below the given path.
   */
  final protected def onEntityRegistration(path : Path)(f : Entity => Any) {
    WorldInterfaceActor ! AddEntityRegistrationListener( self, path )
    creationMessageHandlers = creationMessageHandlers.updated(path, f)
  }

  /**
   *  Applies the passed handler for the entity specified by the given name.
   */
  final protected def handleEntity( name : Symbol )( handler : Option[Entity] => Any ) {
    //TODO Refactor by using LookupEntities, e.g. via (not tested)
    //def _handler(entities: Set[Entity]) = handler(entities.headOption)
    //nonBlockingHandler[Set[Entity]]( LookupEntities( name :: Nil ), _handler )
    nonBlockingHandler[Option[Entity]]( LookupEntity( name :: Nil ), handler )
  }

  /**
   *  Applies the passed handler for all entities that have been registered below the given path so far.
   */
  final protected def requestRegisteredEntities( path : Path )( handler : Set[Entity] => Any ) {
    nonBlockingHandler[Set[Entity]](LookupEntities(path), handler)
  }

  /**
   *  Applies the passed handler for the last entity that has been registered below the given path so far.
   *  If no entity has been registers that way so far,
   *  the passed handler is applied once for the next entity that will be registered below the given path.
   */
  final protected def handleOrWaitForEntityRegistration( path : Path )( f : Entity => Any ) {
    nonBlockingHandler[Entity](OnOneRegistration(path), f)
  }

  /**
   *  Applies the passed handler for all entities that have been registered below the given path so far.
   *  In addition, the passed handler is applied for all entities that will be registered below the given path.
   */
  final protected def handleEntityRegistration(path : Path)(f : Entity => Any){
    requestRegisteredEntities(path){_.foreach(f)}
    onEntityRegistration(path)(f)
  }

  final protected def registerComponentEntity( entity : Entity, cName : Symbol, cType : GroundedSymbol ) {
    registerEntity(Symbols.component.value.toSymbol :: cType.value.toSymbol :: cName :: Nil, entity)
  }
}
