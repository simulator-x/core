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

package simx.core.component

import simx.core.entity.component.{FinalizeComponentConfigMsg, GetInitialConfigValuesMsg, EntityConfigLayer}
import simx.core.entity.description.{EntityAspect, SVal, SValSet}
import simx.core.svaractor.{SimXMessage, SVarActor}
import simx.core.worldinterface.WorldInterfaceHandling
import simx.core.ontology.{GroundedSymbol, types}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.Entity
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling
import scala.annotation.meta.param

/* author: dwiebusch
 * date: 27.08.2010
 */

/**
 *  trait to provide SVarActors with EntityConfigLayer
 *  @author Dennis Wiebusch
 *  @author Stephan Rehfeld
 */
abstract class Component(val componentName : Symbol, val componentType : GroundedSymbol)
  extends SVarActor with EntityConfigLayer with ExecutionStrategySupport with WorldInterfaceHandling with EntityUpdateHandling
  with ComponentManagementRegistration with ComponentIdentification{
  /**
   *  (re)configure the component
   * @param params the configuration params
   */
  protected def configure(params : SValSet)

  //add handler for configuration msgs
  addHandler[ConfigureComponentMessage]{ msg =>
    configure(msg.createParams)
    msg.sender ! ComponentConfigured()
  }

  //add handler to answer get initial value messages
  addHandler[GetInitialConfigValuesMsg]{ msg =>
    addNewRequest(msg.id, msg.e, msg.sender, msg.asp.getProvidings)
    SValSet(types.ComponentName(componentName.name), types.ComponentType(componentType)) ++=
      requestInitialConfigValues( msg.asp.getProvidings, msg.asp, msg.e)
  }

  addHandler[FinalizeComponentConfigMsg]{
    msg => finalizeConfiguration(msg.e)
  }

  /**
   * provide initial values for this component, similar to requestInitialValues for entities
   * @param toProvide the values to be provided
   * @param aspect the component aspect
   * @param e the entity which will represent this component later on
   * @return a SValSet containing the initial configuration for this component
   */
  protected def requestInitialConfigValues( toProvide : Set[ConvertibleTrait[_]], aspect : EntityAspect,
                                            e : Entity ) : SValSet

  /**
   * called when the construction ot the entity representing this component is completed
   * @param e the entity representing this component
   */
  protected def finalizeConfiguration(e : Entity)

  protected def enableAspect(aspect : EntityAspect, e : Entity, success : Boolean => Any) {
    println("[WARNING]: enabling/disabling aspects currently not supported by " + this)
    success(false)
  }

  protected def disableAspect(aspect : EntityAspect, e : Entity, success : Boolean => Any){
    println("[WARNING]: enabling/disabling aspects is currently not supported by " + this)
    success(false)
  }


  // add handler for enabling/disabling aspects
  addHandler[EnableAspect]{
    msg => delayedReplyWith[Boolean](enableAspect(msg.aspect, msg.e, _))()
  }

  addHandler[DisableAspect]{
    msg => delayedReplyWith[Boolean](disableAspect(msg.aspect, msg.e, _))()
  }

  override def toString =
    "Component named " + componentName.name
}

/**
 *
 *  Used to create ConfigureComponentMessages
 */
object ConfigureComponent {
  /**
   *  Creates a ConfigureComponentMessage using Actor.self as sender
   */
  def apply(createParams: SValSet)(implicit actorContext : SVarActor.Ref) = new ConfigureComponentMessage( createParams )
  /**
   *  Creates a ConfigureComponentMessage using Actor.self as sender and params to create a new SValSet
   */
  def apply(params: SVal[_,_]*)(implicit actorContext : SVarActor.Ref) = new ConfigureComponentMessage( new SValSet(params:_*))
}

/**
 *
 *  This message is sent to a component to configure it initially
 *
 * @param createParams A set of CVars that this component needs to be initialized
 *                     Obligatory and optional CVars for individual Components should be defined in their respective documentations.
 */
class ConfigureComponentMessage( val createParams: SValSet )
                               (implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage
/**
 *
 * This message is sent by a component when the configuration process is finished.
 *
 * It will be sended to the sender of the configuration messages.
 *
 */
case class ComponentConfigured( )(implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage

case class EnableAspect(e : Entity, aspect : EntityAspect)(implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage
case class DisableAspect(e : Entity, aspect : EntityAspect)(implicit @(transient @param) actorContext : SVarActor.Ref) extends SimXMessage

//! Exception which is thrown if a requested component was not registered yet
class ComponentDoesNotExistException(val name : Symbol) extends Exception{
  override def toString = "ComponentDoesNotExistException: " + name.name
}
//! Exception which is thrown if a requested component was not registered yet
class ComponentTypeDoesNotExistException(val name : Symbol) extends Exception{
  override def toString = "ComponentTypeDoesNotExistException: " + name.name
}



