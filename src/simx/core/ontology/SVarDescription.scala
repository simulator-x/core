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

import simx.core.entity.description.Semantics
import simx.core.entity.typeconversion._
import simx.core.entity.Entity

import scala.collection.mutable
import scala.reflect.runtime.universe.{typeTag, TypeTag}
import scala.reflect.ClassTag

/* author: dwiebusch
 * date: 14.09.2010
 */

object SVarDescription {
  //! the registered components
  private val registry =
    new mutable.HashMap[Symbol, Set[ConvertibleTrait[_]]] with mutable.SynchronizedMap[Symbol, Set[ConvertibleTrait[_]]]

  /**
   *  retrieve an registered SVarDescription
   * @param typeinfo the typeinfo of the SVarDescription to be retrieved
   * @return the set of matching OntologyMembers
   */
  def apply( typeinfo : Symbol ) : Set[ConvertibleTrait[_]] =
    registry.getOrElse(typeinfo, Set())

  def apply( typeinfo : Symbol, semantics : Symbol ) : Option[ConvertibleTrait[_]] =
    apply(typeinfo).find( _.semantics.toSymbol == semantics )

  private[ontology] def apply[T]( o : ConvertibleTrait[T] )(implicit ct : ClassTag[T]) : SVarDescription[T, T] =
    new SVarDescription[T, T](o.semantics, o.defaultValue(), o.typeinfo, o, o.annotations, o.ontoLink){
      override def definedAt(iri: String) =
        new SVarDescription(semantics, defaultValue(), typeinfo, getBase, annotations, Some(iri))(classTag, baseTag){
          override def getBase = this
        }
    }

  private def register( c : ConvertibleTrait[_] ) {
    registry.update(Symbol(c.typeinfo.toString()),
      registry.getOrElse(Symbol(c.typeinfo.toString()), Set[ConvertibleTrait[_]]()) + c)
  }
}

class SVarDescription[T, B] private[ontology]( val semantics : Semantics, newInstance : => T,
                                               val typeinfo : ClassTag[T],
                                               protected val base : ConvertibleTrait[B],
                                               val annotations : Set[GroundedSymbol] = Set(),
                                               val ontoLink : Option[String] = None)
                                             ( implicit val classTag : ClassTag[T], protected val baseTag : ClassTag[B] )
  extends ConvertibleTrait[T] with Serializable
{
  type baseType = B

  def this( that : SVarDescription[T, B] )(implicit c : ClassTag[T], t : ClassTag[B]) =
    this(that.semantics, that.defaultValue(), that.typeinfo, that.getBase, that.annotations, that.ontoLink)

  def addAnnotations(additionalAnnotations: GroundedSymbol*) =
    new SVarDescription(semantics, newInstance, typeinfo, getBase, annotations ++ additionalAnnotations.toSet, ontoLink)

  //Convenience method
  def withAnnotations(additionalAnnotations: GroundedSymbol*) =
    addAnnotations(additionalAnnotations:_*)

  def createdBy[U : ClassTag, V <: U ](ctor : => V) : SVarDescription[U, B] =
    new SVarDescription(semantics, ctor, scala.reflect.classTag[U], getBase, annotations, ontoLink)

  def definedAt(iri : String)  : SVarDescription[T, B] =
    new SVarDescription(semantics, newInstance, typeinfo, getBase, annotations, Some(iri))

  def as( newSemantics : GroundedSymbol ) : SVarDescription[T, B] =
    as(newSemantics.value)

  def as( newSemantics : Semantics ) : SVarDescription[T, B] =
    new SVarDescription(newSemantics, newInstance, typeinfo, getBase, annotations, ontoLink)

  override def isProvided : Provide[T, B] =
    providedAs[B](getBase)

  override def isRequired : Require[B, T] =
    getBase.requiredAs[T](this)

  def defaultValue() =
    newInstance

  def getBase =
    base

  SVarDescription.register(this)
}

class EntitySVarDescription[T <: Entity] private( that : SVarDescription[T, Entity],
                                                  val ctor : Entity  => T,
                                                  classTag : ClassTag[T])
  extends SVarDescription(that)(classTag, scala.reflect.classTag[Entity]) with Serializable
{
  protected[core] def this(symbol : GroundedSymbol, ctor : (Entity ) => T )(implicit classTag : ClassTag[T]) =
    this(types.Entity.as(symbol).createdBy[T, T ](ctor(new Entity )), ctor, classTag)
}
