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

package simx.core.ontology

import simx.core.entity.description.{GeneralEntityDescription, EntityAspect}
import simx.core.entity.typeconversion.TypeInfo.DataTag
import simx.core.entity.typeconversion._
import simx.core.entity.Entity
import simx.core.worldinterface.{RegisterSVarDescription, WorldInterfaceActor}

import scala.reflect.ClassTag
import simx.core.svaractor.SVarActor

/* author: dwiebusch
 * date: 14.09.2010
 */

object SVarDescription

class SVarDescription[T : ClassTag : DataTag, B] private[ontology]( val semantics : GroundedSymbol,
                                                                    val getBase : ConvertibleTrait[B],
                                                                    val isSValDescription : Boolean,
                                                                    val annotations : Set[Annotation] = Set(),
                                                                    val ontoLink : Option[String] = None,
                                                                    private val doRegister : Boolean = false)
  extends ConvertibleTrait[T] with Serializable
{
  type baseType = B

  override val classTag: ClassTag[T] =
    implicitly[ClassTag[T]]

  override val typeTag: TypeInfo.DataTag[T] =
    implicitly[TypeInfo.DataTag[T]]

  def this( that : SVarDescription[T, B] ) =
    this(that.semantics, that.getBase, that.isSValDescription, that.annotations, that.ontoLink, true)

  def setAnnotations(newAnnotations: Annotation*) =
    new SVarDescription(semantics, getBase, isSValDescription, newAnnotations.toSet, ontoLink)

  //Convenience method
  def withAnnotations(additionalAnnotations: Annotation*) =
    addAnnotations(additionalAnnotations:_*)

  def withType[U : ClassTag : DataTag](c : Class[U]) : SVarDescription[U, B] =
    new SVarDescription(semantics, getBase, isSValDescription, annotations, ontoLink)

  def definedAt(iri : String)  : SVarDescription[T, B] =
    new SVarDescription(semantics, getBase, isSValDescription, annotations, Some(iri))

  def as( newSemantics : GroundedSymbol ) : SVarDescription[T, B] =
    new SVarDescription(newSemantics, getBase, isSValDescription, annotations, ontoLink)

  def asConst =
    new SVarDescription[T, B](semantics, getBase, true, annotations, ontoLink)

  override def isProvided : Provide[T, B] = {
    implicit val ct = getBase.classTag
    //implicit val tt = getBase.typeTag
    providedAs[B](getBase)
  }

  override def isRequired : Require[B, T] =
    getBase.requiredAs[T](this)

  if (doRegister)
    WorldInterfaceActor ! RegisterSVarDescription(this)
}

class EntitySVarDescription[T <: Entity](
  that: SVarDescription[T, Entity],
  val ctor: (Entity, SVarActor) => T,
  alternateAnnotations: Set[Annotation] = Set()
) extends SVarDescription(
  that.semantics,
  that.getBase,
  that.isSValDescription,
  if(alternateAnnotations.isEmpty) that.annotations else alternateAnnotations,
  that.ontoLink,
  doRegister = true)(that.classTag, that.typeTag) with Serializable
{
  protected[ontology] def this(symbol : GroundedSymbol, ctor : (Entity, SVarActor) => T, iri : String)
                              (implicit classTag : ClassTag[T], typeTag : DataTag[T]) =
    this(types.Entity.as(symbol).withType(classTag.runtimeClass.asInstanceOf[Class[T]]).definedAt(iri), ctor, Set[Annotation]())

  def apply(aspects : EntityAspect*) : GeneralEntityDescription[T, T] =
    new SpecificDescription[T](this, aspects.toList, Symbol(classTag.runtimeClass.getSimpleName))

  override def setAnnotations(annotations : Annotation*) =
    new EntitySVarDescription[T](that, ctor, annotations.toSet)
}
