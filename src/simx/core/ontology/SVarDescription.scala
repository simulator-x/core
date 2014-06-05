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
import simx.core.entity.typeconversion._
import simx.core.entity.Entity

import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag
import simx.core.svaractor.SVarActor

/* author: dwiebusch
 * date: 14.09.2010
 */

object SVarDescription {
  //! the registered components
  private val registry =
    new java.util.concurrent.ConcurrentHashMap[Symbol, Set[ConvertibleTrait[_]]]

  /**
   *  retrieve an registered SVarDescription
   * @param typeinfo the typeinfo of the SVarDescription to be retrieved
   * @return the set of matching OntologyMembers
   */
  def apply( typeinfo : Symbol ) : Set[ConvertibleTrait[_]] = registry.get(typeinfo) match {
    case null => Set()
    case set => set
  }

  private[ontology] def apply[T : ClassTag : TypeTag]( o : ConvertibleTrait[T] ) : SVarDescription[T, T] =
    new SVarDescription[T, T](o.semantics, o, o.isSValDescription, o.annotations, o.ontoLink){
      override def definedAt(iri: String) =
        new SVarDescription(semantics, getBase, isSValDescription, annotations, Some(iri)){
          override def getBase = this
        }
    }

  private def register( c : ConvertibleTrait[_] ) {
    var preVal = registry.get(Symbol(c.typeTag.toString()))
    if (preVal == null)
      preVal =  Set[ConvertibleTrait[_]]()
    registry.put(Symbol(c.typeTag.toString()), preVal + c)
  }
}

class SVarDescription[T : ClassTag : TypeTag, B] private[ontology]( val semantics : GroundedSymbol,
                                                                    protected val base : ConvertibleTrait[B],
                                                                    val isSValDescription : Boolean,
                                                                    val annotations : Set[Annotation] = Set(),
                                                                    val ontoLink : Option[String] = None)
  extends ConvertibleTrait[T] with Serializable
{
  type baseType = B

  override val classTag: ClassTag[T] =
    implicitly[ClassTag[T]]

  override val typeTag: TypeTag[T] =
    implicitly[TypeTag[T]]

  def this( that : SVarDescription[T, B] ) =
    this(that.semantics, that.getBase, that.isSValDescription, that.annotations, that.ontoLink)

   def setAnnotations(newAnnotations: Annotation*) =
     new SVarDescription(semantics, getBase, isSValDescription, newAnnotations.toSet, ontoLink)

  //Convenience method
  def withAnnotations(additionalAnnotations: Annotation*) =
    addAnnotations(additionalAnnotations:_*)

  def withType[U : ClassTag : TypeTag](c : Class[U]) : SVarDescription[U, B] =
    new SVarDescription(semantics, getBase, isSValDescription, annotations, ontoLink)

  def definedAt(iri : String)  : SVarDescription[T, B] =
    new SVarDescription(semantics, getBase, isSValDescription, annotations, Some(iri))

  def as( newSemantics : GroundedSymbol ) : SVarDescription[T, B] =
    new SVarDescription(newSemantics, getBase, isSValDescription, annotations, ontoLink)

  def asConst =
    new SVarDescription[T, B](semantics, getBase, true, annotations, ontoLink)

  override def isProvided : Provide[T, B] = {
    implicit val ct = getBase.classTag
    implicit val tt = getBase.typeTag
    providedAs[B](getBase)
  }

  override def isRequired : Require[B, T] =
    getBase.requiredAs[T](this)

  def getBase =
    base

  SVarDescription.register(this)
}

class EntitySVarDescription[T <: Entity]( that : SVarDescription[T, Entity], val ctor : (Entity, SVarActor) => T)
  extends SVarDescription(that)(that.classTag, that.typeTag) with Serializable
{
  protected[ontology] def this(symbol : GroundedSymbol, ctor : (Entity, SVarActor) => T, iri : String)
                              (implicit classTag : ClassTag[T], typeTag : TypeTag[T]) =
    this(types.Entity.as(symbol).withType(classTag.runtimeClass.asInstanceOf[Class[T]]).definedAt(iri), ctor)

  def apply(aspects : EntityAspect*) : GeneralEntityDescription[T, T] =
    new SpecificDescription[T](this, aspects.toList, Symbol(classTag.runtimeClass.getSimpleName))
}
