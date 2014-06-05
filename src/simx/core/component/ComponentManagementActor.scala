/*
 * Copyright 2013 The SIRIS Project
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

package simx.core.component

import simx.core.svaractor.{SimXMessage, SVarActor, SingletonActor}
import scala.collection.mutable
import simx.core.ontology._
import simx.core.ontology
import scala.annotation.meta.param
import akka.actor.ActorSelection

/**
 * @author Stephan Rehfeld
 */
object ComponentManagementActor extends SingletonActor(new ComponentManagementActor, "componentManagementActor")

protected class ComponentManagementActor extends SVarActor {
  //! the registered components by name
  private val registeredComponents = mutable.Map[Symbol, SVarActor.Ref]()
  //! the registered components by type
  private var componentsByType = Map[GroundedSymbol, List[SVarActor.Ref]]()

  private var foreignClusterManagementActors = List[ActorSelection]()

  private var requests = Map[java.util.UUID,Int]()
  private var senderCache = Map[java.util.UUID,SVarActor.Ref]()
  private var componentsLists = Map[java.util.UUID,List[SVarActor.Ref]]()

  addHandler[ComponentManagementActorInCluster] {
    msg => {
      println("Got foreign component manager: " + msg.componentManagementActor)
      foreignClusterManagementActors = foreignClusterManagementActors ::: msg.componentManagementActor :: Nil
    }
  }

  addHandler[GetComponentForName] {
    msg =>
      if( foreignClusterManagementActors.isEmpty || registeredComponents.contains( msg.componentName ) ) {
        msg.sender ! ComponentForNameAnswer( registeredComponents.get(msg.componentName), msg.uuid)
      } else {
        requests = requests + (msg.uuid -> 0)
        senderCache = senderCache + (msg.uuid -> msg.sender)
        for( a <- foreignClusterManagementActors ) {
          a ! InterManagementActorGetComponentForName( msg.componentName, msg.uuid )
          println( "Sending request for " + msg.componentName + " to " + a )
        }
      }
  }

  addHandler[InterManagementActorGetComponentForName] {
    msg => {
      println("Got request, answering with " + registeredComponents.get(msg.componentName) )
      msg.sender ! InterManagementActorComponentForNameAnswer( registeredComponents.get(msg.componentName), msg.uuid)
    }
  }

  addHandler[InterManagementActorComponentForNameAnswer] {
    msg => {
      if( msg.component.isDefined ) {
        senderCache( msg.uuid ) ! ComponentForNameAnswer( msg.component, msg.uuid)
        senderCache = senderCache - msg.uuid
        requests = requests - msg.uuid
      } else {
        if( senderCache.contains( msg.uuid ) ) {
          requests = requests + (msg.uuid -> (requests( msg.uuid) + 1))
          if( requests( msg.uuid ) == foreignClusterManagementActors.size ) {
            senderCache( msg.uuid ) ! ComponentForNameAnswer( None, msg.uuid)
            senderCache = senderCache - msg.uuid
            requests = requests - msg.uuid
          }
        }
      }
    }
  }

  addHandler[GetComponentForType] {
    msg => {
      if( foreignClusterManagementActors.isEmpty ) {
        msg.sender ! ComponentForTypeAnswer( componentsByType.get(msg.componentType), msg.uuid)
      } else {
        requests = requests + (msg.uuid -> 0)
        senderCache = senderCache + (msg.uuid -> msg.sender)
        componentsLists = componentsLists + (msg.uuid -> componentsByType.getOrElse( msg.componentType, List() ) )
        for( a <- foreignClusterManagementActors ) {
          a ! InterManagementActorGetComponentForType( msg.componentType, msg.uuid )
          println( "Sending request for " + msg.componentType + " to " + a )
        }
      }
    }
  }

  addHandler[InterManagementActorGetComponentForType] {
    msg =>
      println("Got request, answering with " + componentsByType.get(msg.componentType) )
      msg.sender ! InterManagementActorComponentForTypeAnswer( componentsByType.get(msg.componentType), msg.uuid )
  }

  addHandler[InterManagementActorComponentForTypeAnswer] {
    msg =>
      requests = requests + ( msg.uuid -> (requests( msg.uuid ) + 1))
      componentsLists = componentsLists + (msg.uuid -> (componentsLists(msg.uuid) ::: msg.components.getOrElse( List() ) ) )
      if( requests( msg.uuid ) == foreignClusterManagementActors.size ) {
        senderCache( msg.uuid ) ! ComponentForTypeAnswer( if( componentsLists( msg.uuid ).isEmpty ) None else Some( componentsLists( msg.uuid ) ), msg.uuid)
        requests = requests - msg.uuid
        componentsLists = componentsLists - msg.uuid
        senderCache = senderCache - msg.uuid
      }
  }

  addHandler[RegisterComponent] {
    msg =>
      if (registeredComponents.get(msg.componentName).isEmpty){
        componentsByType = componentsByType.updated(msg.componentType, msg.component :: componentsByType.getOrElse(msg.componentType, Nil) )
        registeredComponents += msg.componentName -> msg.component
      }
  }
}

trait ComponentManagementRegistration extends SVarActor with ComponentIdentification{
  ComponentManagementActor ! RegisterComponent( self, componentType, componentName )
}



case class InterManagementActorGetComponentForName( componentName : Symbol, uuid : java.util.UUID )
                                                  (implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage

case class InterManagementActorComponentForNameAnswer(  component : Option[SVarActor.Ref], uuid : java.util.UUID )
                                                     (implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage

case class InterManagementActorGetComponentForType( componentType : GroundedSymbol, uuid : java.util.UUID )
                                                  (implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage
case class InterManagementActorComponentForTypeAnswer( components : Option[List[SVarActor.Ref]], uuid : java.util.UUID )
                                                     (implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage

/**
 * @author Stephan Rehfeld
 */
case class RegisterComponent( component : SVarActor.Ref, componentType : GroundedSymbol, componentName : Symbol )
                            (implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage
case class ComponentManagementActorInCluster( componentManagementActor : ActorSelection )
                                            (implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage