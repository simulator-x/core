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

import simx.core.ontology.types.OntologySymbol

import scala.collection.mutable
import simx.core.worldinterface.{ProvideEventMessage, WorldInterfaceActor, WorldInterfaceHandling}
import simx.core.svaractor.SVarActor
import simx.core.ontology


/* author: dwiebusch
* date: 10.09.2010
*/


trait EventHandler extends WorldInterfaceHandling {
  private var _handlers =
    Map[ontology.GroundedSymbol, Set[(Set[SVarActor.Ref], PartialFunction[Event, Boolean], Event => Any)]]()

  private var _knownProviders =
    Map[ontology.GroundedSymbol, Set[SVarActor.Ref]]()

  protected def requestEvent[T]( event : EventDescription ) {
    observe(event, e => {})
  }

  protected def handleEvent(e : Event){
    _handlers.getOrElse(e.name, Set()).foreach( tuple => if (tuple._2(e)) tuple._3(e))
  }

  protected[eventhandling] def observe(desc : EventDescription, handler : Event => Any){
    var requestedFrom = Set[SVarActor.Ref]()
    _knownProviders.get(desc.name) match {
      case None => requireEvent(self, desc)
      case Some(set) =>
        requestedFrom = set
        requestedFrom.foreach( _ ! RegisterEventHandlerMessage(self, desc.name, desc.restriction))
    }
    _handlers = _handlers.updated(desc.name,
      _handlers.getOrElse(desc.name, Set()) +
        ((requestedFrom, desc.restriction.getOrElse({case _ => true} : PartialFunction[Event, Boolean]), handler))
    )
  }

  addHandler[EventProviderMessage]{ msg =>
    _knownProviders = _knownProviders.updated(msg.eventName, _knownProviders.getOrElse(msg.eventName, Set()) ++ msg.provider)
    _handlers = _handlers.updated(msg.eventName, _handlers.getOrElse(msg.eventName, Set()).map { tuple =>
      (msg.provider -- tuple._1).foreach { _ ! RegisterEventHandlerMessage(self, msg.eventName, Some(tuple._2)) }
      (tuple._1 ++ msg.provider, tuple._2, tuple._3)
    })
    msg.event.collect{ case event => handleEvent(event) }
  }

  addHandler[Event]{
    handleEvent
  }
}

trait EventProvider extends SVarActor with WorldInterfaceHandling {
  private type HRPair = (SVarActor.Ref, Option[PartialFunction[Event, Boolean]])
  protected val eventHandlers =  mutable.Map[ontology.GroundedSymbol, Set[HRPair]]()

  /**
   * registers an EventProvider and stores the event it will provide. Furthermore tells all
   * EventHandlers that have requiered the event (more precisely: an event with the same name)
   * of the existence of the Provider
   *
   * @param provider the EventProvider to be registered
   * @param event the event that the provider provides
   */
  final protected def provideEvent(provider : SVarActor.Ref,  eventDesc : EventDescription,  event : Option[Event]) {
    WorldInterfaceActor ! ProvideEventMessage(provider, eventDesc.name, event)
  }

  protected def internalRequireEvent( handler : SVarActor.Ref, name : ontology.GroundedSymbol,
                                      restriction : Option[PartialFunction[Event, Boolean]]) {
    eventHandlers.update(name, eventHandlers.getOrElse(name, Set()) + (handler -> restriction))
  }

  protected def provideEvent( e : EventDescription, event : Option[Event] = None ) {
    if (!registeredEvents.contains(e)){
      registeredEvents = registeredEvents + e
      provideEvent(self, e, event)
    }
  }

  protected def internalRemoveEventHandler(handler: SVarActor.Ref, event : Option[EventDescription] = None) {
    event match {
      case Some(e) =>
        eventHandlers.update(e.name, eventHandlers.getOrElse(e.name, Set[HRPair]()).filterNot(filterFunc(_, handler)))
      case None => for ( (event, handlers) <- eventHandlers){
        eventHandlers.update(event, handlers.filterNot(filterFunc(_, handler )))
      }
    }
  }

  private def filterFunc( pair : HRPair, handler : SVarActor.Ref ) : Boolean =
    pair._1 == handler

  protected var toRemove = Set[SVarActor.Ref]()

  private var registeredEvents = Set[EventDescription]()

  protected[eventhandling] def emitEvent( desc : EventDescription, e : Event ){
    if (!registeredEvents.contains(desc))
      provideEvent(desc, Some(e))
    else
      _emitEvent(e)
  }

  private object publishDevice extends OntologySymbol('publishDevice)

  @deprecated("using emit event directly is deprecated, use the emit function of the associated event description instead", "today")
  protected def emitEvent( e : Event ): Unit = {
    _emitEvent(e)
  }

  private def _emitEvent( e : Event ) {
    toRemove.foreach{ internalRemoveEventHandler(_) }
    toRemove = Set()
    eventHandlers.getOrElse( e.name, Set() ).foreach { pair =>
      if (e.name.equals(publishDevice))
        println("publish device event shall be sent")
      if ( pair._2.collect{ case f => f(e) }.getOrElse(true) && filter(pair._1, e) ) {
        if (e.name.equals(publishDevice))
          println("sending publish device event to " + pair._1)
        if (deadOwners.contains(SVarActor.addressOf(pair._1)))
          toRemove += pair._1
        else
          pair._1 ! e
      }
    }
  }

  protected def filter(handler : SVarActor.Ref, e : Event) : Boolean =
    true

  addHandler[RegisterEventHandlerMessage]{
    (msg : RegisterEventHandlerMessage) => internalRequireEvent( msg.handler, msg.name, msg.restriction )
  }
}

private[core] case class EventProviderMessage( provider : Set[SVarActor.Ref], eventName : ontology.GroundedSymbol, event : Option[Event] = None )
private[core] case class RegisterEventHandlerMessage( handler : SVarActor.Ref, name : ontology.GroundedSymbol, restriction : Option[PartialFunction[Event, Boolean]] )
