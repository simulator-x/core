/*
 * Copyright 2014 The SIRIS Project
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

package simx.core.svaractor.unifiedaccess

import simx.core.entity.Entity
import simx.core.entity.description.SVal
import simx.core.entity.typeconversion.{TypeInfo, ConvertibleTrait}
import simx.core.entity.typeconversion.TypeInfo.DataTag
import simx.core.ontology.types.OntologySymbol
import simx.core.ontology.{types, SValDescription, Annotation}
import simx.core.svaractor.SVarActor
import simx.core.svaractor.TimedRingBuffer.{Unbuffered, BufferMode}
import simx.core.svaractor.semantictrait.base._
import simx.core.worldinterface.WorldInterfaceHandling

import scala.reflect.ClassTag

/**
 *
 * Created by dennis on 12.09.14.
 */
class RelationDescription[S <: Entity : DataTag  : ClassTag, O <: Entity : ClassTag  : DataTag](val leftDesc : ConvertibleTrait[S],
                                                                                                val relationName : GroundedSymbolFeatures,
                                                                                                val rightDesc : ConvertibleTrait[O],
                                                                                                owlLink : String,
                                                                                                val isSymmetric : Boolean,
                                                                                                annotations : Set[Annotation] = Set())
  extends SValDescription(types.Relation.asConst as relationName definedAt owlLink setAnnotations annotations)
{
  private type updateFunc[X <: Entity] = (X => ((X#SelfType => Any) => Unit))

  protected def this(leftDesc : ConvertibleTrait[S], r : RelationDescription[_ >: S, _ >: O], rightDesc : ConvertibleTrait[O],
                     owlLink : String, isSymmetric: Boolean) =
    this(leftDesc, r.relationName, rightDesc, r.ontoLink.get, isSymmetric, r.annotations)

  override def setAnnotations(annotations : Set[Annotation]) =
    new RelationDescription[S, O](leftDesc, relationName, rightDesc, owlLink, isSymmetric, annotations)

  override def setAnnotations(annotations : GroundedSymbolFeatures*) =
    new RelationDescription[S, O](leftDesc, relationName, rightDesc, owlLink, isSymmetric, annotations.map(OntologySymbol(_)).toSet)

  def asRelation[S2 <: S, O2 <: O, X <: Entity : DataTag](s: S2, o : O2, x : X ) =
    new TypedRelation[S, O, X](s, setAnnotations(Set[Annotation](types.RelationSubject(s), types.RelationObject(o))), o, x)

  def restrictTypes[NewS <: S , NewO <: O](newLeft : ConvertibleTrait[NewS], newRight : ConvertibleTrait[NewO]) =
    new RelationDescription[NewS, NewO](newLeft, this, newRight, owlLink, isSymmetric)(newLeft.typeTag, newLeft.classTag, newRight.classTag, newRight.typeTag)

  def restrictSubjectType[SpecialS <: S](specialLeftType : ConvertibleTrait[SpecialS]) =
    restrictTypes(specialLeftType, rightDesc)

  def restrictObjectType[SpecialO <: O](specialRightType : ConvertibleTrait[SpecialO]) =
    restrictTypes(leftDesc, specialRightType)

  def get(right : LeftUnknownTuple[O]) =
    right.getValue.get(LeftRequest[S, O](this, right.getValue))

  def get(left : RightUnknownTuple[S] ) =
    left.getValue.get(RightRequest[S, O](this, left.getValue))

  def get(tuple : (S, O)) =
    tuple._1.get(asRelation(tuple._1, tuple._2, null).description)

  def observe(right : (Unknown, O), ignoredWriters : Set[SVarActor.Ref]) =
    right._2.observe(LeftRequest[S, O](this, right._2), ignoredWriters)

  def observe(left : RightUnknownTuple[S])  : ObservableAccessSet[Relation, O] =
    observe(left, Set[SVarActor.Ref]())

  def observe(left : RightUnknownTuple[S], ignoredWriters : Set[SVarActor.Ref])  : ObservableAccessSet[Relation, O] =
    left.getValue.observe(RightRequest(this, left.getValue), ignoredWriters)

  def observe(right : LeftUnknownTuple[O]) : ObservableAccessSet[Relation, S] =
    observe(right, Set[SVarActor.Ref]())

  def observe(right : LeftUnknownTuple[O], ignoredWriters : Set[SVarActor.Ref]) : ObservableAccessSet[Relation, S] =
    right.getValue.observe(LeftRequest(this, right.getValue), ignoredWriters)

  def observe(tuple : (S, O)) =
    tuple._1.observe(asRelation(tuple._1, tuple._2, null).description)

  def set(subj : S, obj : O, bufferMode : BufferMode = Unbuffered)(implicit actor : WorldInterfaceHandling with EntityUpdateHandling) : Unit =
    asRelation(subj, obj, subj).publish(_ => {}, bufferMode)

  def set(tuple : ( S, O ))(implicit actor : WorldInterfaceHandling  with EntityUpdateHandling) : Unit =
    set(tuple, Unbuffered)

  def set(tuple : ( S, O ), bufferMode : BufferMode)(implicit actor : WorldInterfaceHandling  with EntityUpdateHandling) : Unit =
    set(tuple._1, tuple._2, bufferMode)

  def remove(subj : S, obj : O)(implicit actor : WorldInterfaceHandling  with EntityUpdateHandling) : Unit =
    asRelation(subj, obj, subj).remove(_ => {})

  def remove(tuple : ( S, O ))(implicit actor : WorldInterfaceHandling  with EntityUpdateHandling) : Unit =
    remove(tuple._1, tuple._2)

  def ?  : S => LeftRelationPart[S, O] =
    LeftRelationPart(this, _)

  def ->(x : Unknown) : S => LeftRelationPart[S, O] =
    LeftRelationPart(this, _)

  def ->(x : O) =
    RightRelationPart[S, O](this, x)

  def apply[X <: StateParticleAccess](in : TypedRelation[S, O, X])(implicit ct : ClassTag[TypedRelation[S, O, X]], dt : DataTag[TypedRelation[S, O, X]]) =
    SVal(withType(classOf[TypedRelation[S, O, X]]), valueDescription)(in)

  override def toString: String =
    sVarIdentifier.name + " (" + typeTag.toString() + annotationsString + ")"

  private def annotationsString: String = if (annotations.isEmpty) ""
  else " (with annotations " + annotations.map { annotation =>
    createPrefixStringFrom(annotation) + createSuffixStringFrom(annotation)
  }.mkString(" and ")

  private val relationRoles: Set[TypeInfo[_,_]] = Set(types.RelationSubject,types.RelationObject)

  private def createPrefixStringFrom(annotation: Annotation): String =
    if(relationRoles.contains(annotation.typedSemantics))
      annotation.value.asInstanceOf[Entity].getSimpleName
    else annotation.value.toString

  private def createSuffixStringFrom(annotation: Annotation): String =
    if (annotation.typedSemantics == OntologySymbol) ""
    else " (" + annotation.typedSemantics.sVarIdentifier.name + ")"
}
