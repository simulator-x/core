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
import simx.core.ontology.types

/**
 *
 * Created by dennis on 12.09.14.
 */
sealed abstract class Request[T <: Entity, V <: Entity] (desc : RelationDescription[_, _], val isLeft : Boolean) {
  def accessValue(r: Relation) : T
  def getKnownValue : V

  val description : RelationDescription[_ ,_] =
    desc.setAnnotations( if (isLeft) types.RelationObject(getKnownValue) else types.RelationSubject(getKnownValue))
}


trait PartialRequest[Y <: Entity]{
  def asRequest : Request[Y, _]
  def description : RelationDescription[_, _]
}

case class LeftRequest[S <: Entity, O <: Entity] protected[unifiedaccess]
(desc : RelationDescription[ S,  O], getKnownValue: O) extends Request[S, O](desc, true)
{
  override def accessValue(r: Relation) : S =
    types.Entity.convert(desc.leftDesc)(r.getSubject)
}

case class RightRequest[S <: Entity, O <: Entity] protected[unifiedaccess]
(desc : RelationDescription[S, O], getKnownValue: S) extends Request[O, S](desc, false)
{
  override def accessValue(r : Relation) : O =
    types.Entity.convert(desc.rightDesc)(r.getObject)
}
