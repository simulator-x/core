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

package simx.core.entity.component

import scala.reflect.{classTag, ClassTag}
import simx.core.entity.description.{NamedSValSet, EntityAspect}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.component.Component
import simx.core.ontology
import simx.core.ontology._
import akka.actor.Props
import simx.core.component.remote.RemoteActor

/**
 * User: dwiebusch
 * Date: 17.11.13
 * Time: 17:41
 */
abstract class ComponentAspect[T <: Component : ClassTag] protected(val props : Props,
                                                                    val cName : Symbol,
                                                                    cType : ontology.GroundedSymbol)
  extends EntityAspect(cType, Symbols.component) with Serializable
{
  // initial check
  //assert(!getProvidings.exists(InnerComponentAspect.innerFeatures.contains))

  val nodeName : Option[String] =
    None

  def this(cType : ontology.GroundedSymbol, name : Symbol, args : Seq[Any] = Seq()) =
    this(Props(classTag[T].runtimeClass, name +: args :_*), name, cType)

  final def runtimeClass : Class[T] =
    classTag[T].runtimeClass.asInstanceOf[Class[T]]

  final def getFeatures =
    getComponentFeatures + ontology.types.ComponentType + ontology.types.ComponentName

  final def getProvidings =
    getFeatures

  def on(name : String) =
    onNode(name)

  def onNode(name : String) : ComponentAspect[T] =
    RemoteComponentAspect(this, name)

  def getComponentFeatures : Set[ConvertibleTrait[_]]

  /**
   * a print bit more information on this aspect
   * @return a string containing information on this aspect
   */
  override def toString: String =
    "ComponentAspect for component " + runtimeClass.getSimpleName + " on " + nodeName.getOrElse("localhost")
}

case class RemoteComponentAspect[T <: Component : ClassTag] protected[component](base : ComponentAspect[T], node : String)
  extends ComponentAspect[T](base.props, base.cName, base.componentType)
{
  RemoteActor.checkRemoting()

  override val nodeName: Option[String] =
    Some(node)

  def getCreateParams: NamedSValSet =
    base.getCreateParams

  def getComponentFeatures =
    base.getComponentFeatures
}

protected[core] object InnerComponentAspect{
  def innerFeatures : Set[ConvertibleTrait[_]] =
    Set(ontology.types.Component.asConst, types.Name.asConst)
}

protected[component] case class InnerComponentAspect[T <: Component](cAsp : ComponentAspect[T])
  extends EntityAspect(Symbols.component, Symbols.component)
{
  // ensure the component creation component does exist
  protected[component] val props =
    cAsp.props

  def getFeatures: Set[ConvertibleTrait[_]] =
    InnerComponentAspect.innerFeatures

  def getProvidings: Set[ConvertibleTrait[_]] =
    getFeatures

  def getCreateParams: NamedSValSet =
    NamedSValSet(aspectType)

  /**
   * a print bit more information on this aspect
   * @return a string containing information on this aspect
   */
  override def toString: String =
    "InnerComponentAspect for " + cAsp.toString
}