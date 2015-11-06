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

import simx.core.entity.description.SVal.SValType
import simx.core.entity.description._
import simx.core.entity.typeconversion.TypeInfo.DataTag
import simx.core.entity.typeconversion._
import simx.core.entity.Entity
import simx.core.ontology.types.OntologySymbol
import simx.core.svaractor.semantictrait.base._
import simx.core.worldinterface.{RegisterSVarDescription, WorldInterfaceActor}

import scala.reflect.ClassTag
import simx.core.svaractor.SVarActor

/* author: dwiebusch
 * date: 14.09.2010
 */

object SValDescription
object SVarDescriptionSymbol extends OntologySymbol('SVarDescription)




class SValDescription[T : ClassTag : DataTag, B, X <: Base, S <: Thing] private[ontology]( val classType : Class[T],
                                                                                           vd : ValueDescription[X, S],
                                                                                           val getBase : ConvertibleTrait[B],
                                                                                           val isSValDescription : Boolean,
                                                                                           val annotations : Set[Annotation] = Set(),
                                                                                           val ontoLink : Option[String] = None,
                                                                                           doRegister : Boolean = false)
  extends SemanticType[T, X, S](classType, vd, doRegister) with ConvertibleTrait[T] with Serializable
{

  //simx.core.entity.description.SVal[
  // String,
  // simx.core.entity.typeconversion.TypeInfo[String,String],
  // simx.core.svaractor.semantictrait.base.ValueDescription[simx.core.svaractor.semantictrait.base.Base,simx.core.ontology.Symbols.string.SymbolType],
  // simx.core.ontology.Symbols.material.SymbolType];

  type baseType = B
  type SemanticSValType <: SVal[T, TypeInfo[T, T], X, S]

  override def apply(value: T): SemanticSValType =
    apply(value, -1L)

  def apply(value: T, timestamp : Long) : SemanticSValType = {
    apply(value, timestamp, Nil)
  }

  def apply(value: T, timestamp : Long, history: HistoryStorage.HistoryType[T]) : SemanticSValType with SValHistory[DataType, S, SemanticSValType]= {
    SVal(this, valueDescription.setAnnotations(annotations), timestamp, history)(value)(classTag).asInstanceOf[SemanticSValType with SValHistory[DataType, S, SemanticSValType]]
  }

  override val semantics: GroundedSymbolBase[S] = vd.groundedSymbol

  override val classTag: ClassTag[T] =
    implicitly[ClassTag[T]]

  override val typeTag: TypeInfo.DataTag[T] =
    implicitly[TypeInfo.DataTag[T]]

  def this( that : SValDescription[T, B, X, S] ) =
    this(that.classType, that.valueDescription, that.getBase, that.isSValDescription, that.annotations, that.ontoLink, true)

  def setAnnotations(newAnnotations: Set[Annotation]) =
    new SValDescription(classType, valueDescription, getBase, isSValDescription, newAnnotations, ontoLink)

  def setAnnotations(newAnnotations: GroundedSymbolFeatures*) =
    new SValDescription[T, B, X, S](classType, valueDescription, getBase, isSValDescription, newAnnotations.map(OntologySymbol(_)).toSet, ontoLink)

  //Convenience method
  def withAnnotations(additionalAnnotations: Set[Annotation]) =
    addAnnotations(additionalAnnotations)

  //TODO fix the following 'type clutter'
  //def withAnnotations(additionalAnnotations: simx.core.ontology.Annotation*): simx.core.entity.typeconversion.ConvertibleTrait[T] at line 96 and
  //def withAnnotations(additionalAnnotations: simx.core.svaractor.semantictrait.base.GroundedSymbolFeatures*): simx.core.entity.typeconversion.ConvertibleTrait[T] at line 99
  //have same type after erasure: (additionalAnnotations: Seq)simx.core.entity.typeconversion.ConvertibleTrait
  def withAnnotations(additionalAnnotation: Annotation, additionalAnnotations: Annotation*) =
    addAnnotations(additionalAnnotations.toSet + additionalAnnotation)

  def withAnnotations(additionalAnnotations: GroundedSymbolFeatures*) =
    addAnnotations(additionalAnnotations :_*)

  def +@(additionalAnnotations: GroundedSymbolFeatures*) =
    addAnnotations(additionalAnnotations :_*)

  def withType[U : ClassTag : DataTag](c : Class[U]) : SValDescription[U, B, X, S] =
    new SValDescription(c, valueDescription, getBase, isSValDescription, annotations, ontoLink)

  def definedAt(iri : String)  : SValDescription[T, B, X, S] =
    new SValDescription(classType, valueDescription, getBase, isSValDescription, annotations, Some(iri))

  private[ontology] def forceAs[NewX <: Base, NewS <: Thing]( newSemantics : ValueDescription[NewX, NewS] ) : SValDescription[T, B, NewX, NewS] =
    new SValDescription[T, B, NewX, NewS](classType, newSemantics, getBase, isSValDescription, annotations, ontoLink)

  def as[NewS <: GroundedSymbolFeatures]( newSemantics : NewS ) : SValDescription[T, B, ValueDescription[X, S], NewS#SymbolType] =
    new SValDescription(classType, valueDescription as newSemantics, getBase, isSValDescription, annotations, ontoLink)

  def asConst =
    new SValDescription[T, B, X, S](classType, valueDescription, getBase, true, annotations, ontoLink)

  def convertedFrom[O](that : SVal.SValType[O]) : SValType[T] =
    apply(that as this)

  override def isProvided : Provide[T, B] =
    providedAs[B](getBase)(getBase.classTag)

  override def isRequired : Require[B, T] =
    getBase.requiredAs[T](this)

  if (doRegister)
    WorldInterfaceActor ! RegisterSVarDescription(this)
}

class EntitySValDescription( that: SValDescription[Entity, Entity, Base, Thing],
                             val ctor: (Entity, SVarActor) => Entity,
                             alternateAnnotations: Set[Annotation] = Set()
                             ) extends SValDescription(
  that.classType,
  that.valueDescription,
  that.getBase,
  that.isSValDescription,
  if(alternateAnnotations.isEmpty) that.annotations else alternateAnnotations,
  that.ontoLink,
  doRegister = true)(that.classTag, that.typeTag) with Serializable
{
  protected[ontology] def this(symbol : ValueDescription[Base, Thing], ctor : (Entity, SVarActor) => Entity, iri : String)
                              (implicit classTag : ClassTag[Entity], typeTag : DataTag[Entity]) =
    this(types.Entity.forceAs(symbol).withType(classTag.runtimeClass.asInstanceOf[Class[Entity]]).definedAt(iri), ctor, Set[Annotation]())

  def apply(aspects : EntityAspect*) : GeneralEntityDescription =
    new SpecificDescription(this, aspects.toList, Symbol(classTag.runtimeClass.getSimpleName))

  def this() = this( BaseValueDescription(_PlainEntity),
    (entity: Entity, svarActor: SVarActor) => new Entity(entity)(svarActor), types.Entity.ontoLink.get)

  override def setAnnotations(annotations : Set[Annotation]) =
    new EntitySValDescription(that, ctor, annotations)

  def setAnnotations(annotations : GroundedSymbolFeatures) =
    new EntitySValDescription(that, ctor, Set[Annotation](OntologySymbol(annotations)))
}
