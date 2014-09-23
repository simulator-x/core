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
import simx.core.entity.typeconversion.TypeInfo._

import scala.reflect.ClassTag

/**
 *
 * Created by dennis on 12.09.14.
 */
sealed trait PartialRelation[-X <: EntityRelationAccess, S <: Entity, O <: Entity]{
  def complete[X2 <: X : DataTag](missingPart : X2) : TypedRelation[S, O, X2] = complete(missingPart, (x  : X2#SelfType) => {})
  def complete[X2 <: X : DataTag](missingPart : X2, handler : X2#SelfType => Any) : TypedRelation[S, O, X2]
}

case class LeftRelationPart[S <: Entity : ClassTag, O <: Entity : ClassTag ] private[unifiedaccess]
(description : RelationDescription[S, O], subj : S) extends PartialRelation[O, S, O] with PartialRequest[O]
{
  def complete[O2 <: O : DataTag](obj: O2, handler : O2#SelfType => Any) =
    description.asRelation[S, O, O2](subj, obj, x => x(obj)(handler))
  def asRequest: Request[O, S] = RightRequest(description, subj)
}

case class RightRelationPart[S <: Entity : ClassTag, O <: Entity : ClassTag] private[unifiedaccess]
(description : RelationDescription[S, O], obj : O) extends PartialRelation[S, S, O] with PartialRequest[S]
{
  def complete[S2 <: S : DataTag](subj: S2, handler : S2#SelfType => Any) =
    description.asRelation[S, O, S2](subj, obj, x => x(subj).apply(handler))
  def asRequest: Request[S, O] = LeftRequest(description, obj)
}

