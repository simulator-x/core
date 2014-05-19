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

import scala.reflect.ClassTag
import simx.core.entity.Entity
import scala.reflect.runtime.universe.TypeTag
import simx.core.svaractor.SVarActor


/**
 * Created by dwiebusch on 10.03.14
 */
/**
 * Created by dwiebusch on 04.03.14
 */
abstract class Unknown private[unifiedaccess]()
abstract class UnknownTuple private[unifiedaccess]()
protected case class LeftUnknownTuple[+T](_2 : T) extends UnknownTuple
protected case class RightUnknownTuple[+T](_1 : T) extends UnknownTuple

object ? extends Unknown{
  def is[S <: Entity : TypeTag, O <: Entity : TypeTag](rDesc : RelationDescription[S, O]) =
    ->(rDesc)

  def ->[S <: Entity : TypeTag, O <: Entity : TypeTag ](rDesc : RelationDescription[S, O])  =
    (a : O, b : EntityUpdateHandling) => LeftRequest(rDesc, a)

  def ->[T <: RelationAccess : ClassTag](x : T) =
    LeftUnknownTuple(x)

  def observe[T <: Entity, U <: Entity](request : RightRelationPart[T, U], ignoredWriters : Set[SVarActor.Ref] = Set()) =
    request.obj.observe(request.asRequest, ignoredWriters)

  def get[T <: Entity : ClassTag, U <: Entity](request : RightRelationPart[T, U]) =
    request.obj.get(request.asRequest)
}


