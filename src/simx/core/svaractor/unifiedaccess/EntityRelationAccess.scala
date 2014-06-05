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

import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.Entity
import scala.reflect.ClassTag
import simx.core.svaractor.SVarActor

/**
 * Created by dwiebusch on 04.03.14
 */
trait EntityRelationAccess extends RelationAccess with StateParticleAccess{
  final def get[T <: Entity : ClassTag](request : this.type => PartialRequest[T]) : AccessSet[Relation, T] =
    get(request(this).asRequest)

  final def get[T <: Entity : ClassTag](request :  Request[T, _]) : AccessSet[Relation, T] =
    new AccessSet[Relation, T](access(request.description, _), _.accessValue, request.accessValue)

  final def get[T <: Entity](r : RightRelationPart[SelfType, T]) : AccessSet[Relation, Relation] =
    new AccessSet[Relation, Relation](access(r.complete(asSelfType).description, _), _.accessValue, x => x)

  final def observe[T <: Entity](request : this.type => PartialRequest[T]) : ObservableAccessSet[Relation, T] =
    observe(request, Set[SVarActor.Ref]())

  final def observe[T <: Entity](request : this.type => PartialRequest[T], ignored : Set[SVarActor.Ref]) : ObservableAccessSet[Relation, T] =
    observe(request(this).asRequest, ignored)

  final def observe[T <: Entity](request : Request[T, _]) : ObservableAccessSet[Relation, T] =
    observe(request, Set[SVarActor.Ref]())

  final def observe[T <: Entity](r : RightRelationPart[SelfType , T]) : ObservableAccessSet[Relation, Relation] =
    observe(r, Set[SVarActor.Ref]())

  final def observe[T <: Entity](request : Request[T, _], ignored : Set[SVarActor.Ref]) : ObservableAccessSet[Relation, T] =
    _observe[T](request.description, ignored, request.accessValue)

  final def observe[T <: Entity](r : RightRelationPart[SelfType , T], ignored : Set[SVarActor.Ref]) : ObservableAccessSet[Relation, Relation] =
    _observe[Relation](r.complete(asSelfType).description, ignored, x => x)

  private def _observe[T](desc : ConvertibleTrait[Relation], ignored : Set[SVarActor.Ref],
                          convert : Relation => T) : ObservableAccessSet[Relation, T] =
    new ObservableAccessSet(this, access(desc, _), _.observe(ignored), convert)

  def remove[T](c : ConvertibleTrait[T], handler : SelfType => Any = _ => {})(implicit actor : EntityUpdateHandling)
}
