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

package simx.core.worldinterface.eventhandling

import scala.collection.mutable
import simx.core.ontology.GroundedSymbol
import simx.core.entity.description.Semantics
import simx.core.worldinterface.WorldInterfaceHandling


/* author: dwiebusch
* date: 10.09.2010
*/


trait EventHandler extends WorldInterfaceHandling {
  protected def handleEvent(e : Event)

  protected def requestEvent[T]( event : EventDescription ) {
    requireEvent(this, event)
  }

  addHandler[EventProviderMessage]{
    (msg : EventProviderMessage) => msg.provider.foreach{ _.self ! RegisterEventHandlerMessage(this, msg.event) }
  }

  addHandler[Event]{
    handleEvent
  }
}

trait EventProvider extends WorldInterfaceHandling {
  private type HRPair = (EventHandler, Option[PartialFunction[Event, Boolean]])
  protected val eventHandlers =  mutable.Map[GroundedSymbol, Set[HRPair]]()

  protected def internalRequireEvent( handler : EventHandler, event : EventDescription ) {
    //TODO: Test the updated partial functions!
    eventHandlers.update(event.name, eventHandlers.get(event.name) match {
      case None => Set(handler -> event.restriction)
      case Some(set) =>
        if (event.restriction.isEmpty && set.exists{ h => h._1 == handler && h._2.isEmpty})
          set
        else
          set + ( set.find( _._1 == handler ) match {
            case Some((_, Some(func))) =>
              handler -> Some( event.restriction.collect{ case f => f.orElse(func)}.getOrElse{ func.orElse{ case _ => true} } )
            case _ =>
              handler -> event.restriction
          })
    })
  }

  protected def provideEvent( e : EventDescription ) {
    provideEvent(this, e)
  }

  protected def internalRemoveEventHandler(handler: EventHandler, event : Option[EventDescription] = None) { event match {
    case Some(e) =>
      eventHandlers.update(e.name, eventHandlers.getOrElse(e.name, Set[HRPair]()).filterNot(filterFunc(_, handler)))
    case None => for ( (event, handlers) <- eventHandlers)
      eventHandlers.update(event, handlers.filterNot(filterFunc(_, handler )))
  } }

  private def filterFunc( pair : HRPair, handler : EventHandler ) : Boolean =
    pair._1 == handler

  protected def emitEvent( e : Event ) = eventHandlers get e.name collect {
    case s => s.foreach{ pair =>
      if (e.name.equals(simx.core.ontology.types.OntologySymbol(new Semantics{ def toSymbol : Symbol= 'publishDevice })))
        println("publish device event shall be sent")
      if ( pair._2.collect{ case f => f(e) }.getOrElse(true) && filter(pair._1, e) ) {
        if (e.name.equals(simx.core.ontology.types.OntologySymbol(new Semantics{ def toSymbol : Symbol= 'publishDevice })))
          println("sending publish device event to " + pair._1)
        pair._1.self ! e
      }
    }
  }

  protected def filter(handler : EventHandler, e : Event) : Boolean =
    true

  addHandler[RegisterEventHandlerMessage]{
    (msg : RegisterEventHandlerMessage) => internalRequireEvent( msg.handler, msg.event )
  }
}

private[core] case class EventProviderMessage( provider : Set[EventProvider], event : EventDescription )
private[core] case class RegisterEventHandlerMessage( handler : EventHandler, event : EventDescription )
private[core] case class RegistrationOK( provider : EventProvider, event : EventDescription )
