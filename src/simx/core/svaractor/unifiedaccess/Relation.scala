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
import simx.core.ontology.{types, SVarDescription, GroundedSymbol}
import simx.core.worldinterface.WorldInterfaceHandling
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.svaractor.SVarActor
import reflect.runtime.universe.TypeTag
import reflect.ClassTag


final class Relation(val subj : Entity, d : RelationDescription[_ <: Entity, _ <: Entity], val obj : Entity){
  val description = d.setAnnotations(types.RelationSubject(subj), types.RelationObject(obj))

  def publish()(implicit context : WorldInterfaceHandling with EntityUpdateHandling){
    context setRelation description(this)
  }

  def remove()(implicit context : WorldInterfaceHandling with EntityUpdateHandling){
    context removeRelation description(this)
  }

  override def toString: String =
    subj.getSimpleName + " " + description.semantics.value.toString + " " + obj.getSimpleName
}

abstract class RelationDescription[S <: Entity : TypeTag : ClassTag, O <: Entity : TypeTag : ClassTag]
(val leftDesc : ConvertibleTrait[S], relationName : GroundedSymbol, val rightDesc : ConvertibleTrait[O])
  extends SVarDescription(types.Relation.asConst as relationName)
{
  def get(right : LeftUnknownTuple[O]) =
    right.getValue.get(LeftRequest[S, O](this, right.getValue))

  def get(left : RightUnknownTuple[S] ) =
    left.getValue.get(RightRequest[S, O](this, left.getValue))

  def get(tuple : (S, O)) =
    tuple._1.get(RightRelationPart[S#SelfType, O](this, tuple._2))

  def observe(right : (Unknown, O), ignoredWriters : Set[SVarActor.Ref]) =
    right._2.observe(LeftRequest[S, O](this, right._2), ignoredWriters)

  def observe(left : RightUnknownTuple[S], ignoredWriters : Set[SVarActor.Ref]) =
    left.getValue.observe(RightRequest(this, left.getValue), ignoredWriters)

  def observe(tuple : (S, O)) =
    tuple._1.observe(RightRelationPart[S#SelfType, O](this, tuple._2))

  def set(subj : S, obj : O)(implicit actor : WorldInterfaceHandling with EntityUpdateHandling) : Unit =
    new Relation(subj, this, obj).publish()

  def set(tuple : ( S, O ))(implicit actor : WorldInterfaceHandling  with EntityUpdateHandling) : Unit =
    set(tuple._1, tuple._2)

  def remove(subj : S, obj : O)(implicit actor : WorldInterfaceHandling  with EntityUpdateHandling) : Unit =
    new Relation(subj, this, obj).remove()

  def remove(tuple : ( S, O ))(implicit actor : WorldInterfaceHandling  with EntityUpdateHandling) : Unit =
    remove(tuple._1, tuple._2)

  def ?  : S => LeftRelationPart[S, O] =
    LeftRelationPart(this, _)

  def ->(x : Unknown) : S => LeftRelationPart[S, O] =
    LeftRelationPart(this, _)

  def ->(x : O) =
    RightRelationPart[S, O](this, x)
}

sealed trait PartialRelation[-X, S <: Entity, O <: Entity]{
  def complete(missingPart : X) : Relation
}

case class LeftRelationPart[S <: Entity : ClassTag, O <: Entity : ClassTag : TypeTag] private[unifiedaccess]
(desc : RelationDescription[S, O], subj : S) extends PartialRelation[O, S, O] with PartialRequest[O]
{
  def complete(obj: O) = new Relation(subj, desc, obj)
  def asRequest: Request[O, S] = RightRequest(desc, subj)
}

case class RightRelationPart[S <: Entity : ClassTag : TypeTag, O <: Entity : ClassTag] private[unifiedaccess]
(desc : RelationDescription[_ <: S, _ <: O], obj : O) extends PartialRelation[S, S, O] with PartialRequest[S]
{
  def complete(subj: S) = new Relation(subj, desc, obj)
  def asRequest: Request[S, O] = LeftRequest(desc, obj)
}


sealed abstract class Request[T <: Entity, V <: Entity] (val desc : RelationDescription[_, _], val isLeft : Boolean) {
  def accessValue(r: Relation) : T
  def getKnownValue : V

  val description : ConvertibleTrait[Relation] =
    desc.setAnnotations( if (isLeft) types.RelationObject(getKnownValue) else types.RelationSubject(getKnownValue))
}

sealed trait PartialRequest[Y <: Entity]{
  def asRequest : Request[Y, _]
}

case class LeftRequest[S <: Entity: TypeTag, O <: Entity] protected[unifiedaccess]
(d : RelationDescription[_ <: S, _ <: O], getKnownValue: O) extends Request[S, O](d, true)
{
  override def accessValue(r: Relation) : S =
    types.Entity.convert(d.leftDesc)(r.subj)
}

case class RightRequest[S <: Entity, O <: Entity: TypeTag] protected[unifiedaccess]
(d : RelationDescription[S, O], getKnownValue: S) extends Request[O, S](d, false)
{
  override def accessValue(r : Relation) : O =
    types.Entity.convert(d.rightDesc)(r.obj)
}
