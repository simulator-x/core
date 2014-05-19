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

import simx.core.svaractor.{SimXMessage, SVarActor}
import simx.core.ontology._
import scala.annotation.meta.param


/**
 * @author Stephan Rehfeld
 */
trait ComponentHandling extends SVarActor {
  private[component] var componentForNameHandler = Map[java.util.UUID,(Option[SVarActor.Ref])=>Unit]()
  private[component] var componentForTypeHandler = Map[java.util.UUID,(Option[List[SVarActor.Ref]])=>Unit]()

  final protected def componentForName( componentName : Symbol, handler : ((Option[SVarActor.Ref]) => Unit ) ) {
    val uuid = java.util.UUID.randomUUID()
    ComponentManagementActor ! GetComponentForName( componentName, uuid )
    componentForNameHandler = componentForNameHandler + (uuid -> handler)
  }

  final protected def componentForType( componentType : GroundedSymbol, handler : (Option[List[SVarActor.Ref]]) => Unit  ) {
    val uuid = java.util.UUID.randomUUID()
    ComponentManagementActor ! GetComponentForType( componentType, uuid )
    componentForTypeHandler = componentForTypeHandler + (uuid -> handler)
  }

  final protected def registerComponent( component : SVarActor.Ref, componentType : GroundedSymbol, componentName : Symbol ) {
    ComponentManagementActor ! RegisterComponent( component, componentType, componentName )
  }

  addHandler[ComponentForNameAnswer] {
    msg =>
      val handler = componentForNameHandler( msg.uuid )
      componentForNameHandler = componentForNameHandler - msg.uuid
      handler( msg.component )
  }

  addHandler[ComponentForTypeAnswer] {
    msg =>
      val handler = componentForTypeHandler( msg.uuid )
      componentForTypeHandler = componentForTypeHandler - msg.uuid
      handler( msg.components )
  }

}

/**
 * @author Stephan Rehfeld
 */
case class GetComponentForName( componentName : Symbol, uuid : java.util.UUID )
                              ( implicit @(transient @param) actor : SVarActor.Ref ) extends SimXMessage

/**
 * @author Stephan Rehfeld
 */
case class ComponentForNameAnswer( component : Option[SVarActor.Ref], uuid : java.util.UUID )
                                 ( implicit @(transient @param) actor : SVarActor.Ref ) extends SimXMessage

/**
 * @author Stephan Rehfeld
 */
case class GetComponentForType( componentType : GroundedSymbol, uuid : java.util.UUID )(implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage

/**
 * @author Stephan Rehfeld
 */
case class ComponentForTypeAnswer( components : Option[List[SVarActor.Ref]], uuid : java.util.UUID )
                                 (implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage