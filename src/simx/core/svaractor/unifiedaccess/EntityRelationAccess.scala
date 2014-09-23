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
import simx.core.svaractor.TimedRingBuffer.{Time, Now}
import simx.core.svaractor.{AccessMethod, GetClosest, StateParticle, SVarActor}
import simx.core.entity.Entity
import scala.reflect.ClassTag

/**
 * Created by dwiebusch on 04.03.14
 */
trait EntityRelationAccess extends RelationAccess with StateParticleAccess{
  //gets
  final def get[T <: Entity : ClassTag](request : this.type => PartialRequest[T]) : AccessSet[Relation, T] =
    get(request, Now, GetClosest)

  final def get[T <: Entity : ClassTag](request : this.type => PartialRequest[T], at : Time ) : AccessSet[Relation, T] =
    get(request, at, GetClosest)

  final def get[T <: Entity : ClassTag](request : this.type => PartialRequest[T], accessMethod : AccessMethod) : AccessSet[Relation, T] =
    get(request, Now, accessMethod)

  final def get[T <: Entity : ClassTag](request : this.type => PartialRequest[T],
                                        at : Time, accessMethod : AccessMethod) : AccessSet[Relation, T] =
    get(request(this).asRequest, at, accessMethod)

  //
  final def get[T <: Entity : ClassTag](request :  Request[T, _]) : AccessSet[Relation, T] =
    get(request, Now, GetClosest)

  final def get[T <: Entity : ClassTag](request :  Request[T, _], at : Time) : AccessSet[Relation, T] =
    get(request, at, GetClosest)

  final def get[T <: Entity : ClassTag](request :  Request[T, _], accessMethod : AccessMethod) : AccessSet[Relation, T] =
    get(request, Now, accessMethod)

  final def get[T <: Entity : ClassTag](request :  Request[T, _], at : Time, accessMethod : AccessMethod) : AccessSet[Relation, T] =
    _get(request.description, request.accessValue, at, accessMethod)

  //
  final def get[T <: Entity](r : RightRelationPart[_ >: SelfType , T]) : AccessSet[Relation, Relation] =
    get(r, Now, GetClosest)

  final def get[T <: Entity](r : RightRelationPart[_ >: SelfType , T], at : Time) : AccessSet[Relation, Relation] =
    get(r, at, GetClosest)

  final def get[T <: Entity](r : RightRelationPart[_ >: SelfType , T], accessMethod : AccessMethod) : AccessSet[Relation, Relation] =
    get(r, Now, accessMethod)

  final def get[T <: Entity](r : RightRelationPart[_ >: SelfType , T], at : Time, accessMethod : AccessMethod) : AccessSet[Relation, Relation] =
    _get(r.complete(asSelfType).description, x => x, at, accessMethod)


  //observes
  final def observe[T <: Entity](request : this.type => PartialRequest[T]) : ObservableAccessSet[Relation, T] =
    observe(request, Set[SVarActor.Ref]())

  final def observe[T <: Entity](request : this.type => PartialRequest[T], ignored : Set[SVarActor.Ref]) : ObservableAccessSet[Relation, T] =
    observe(request(this).asRequest, ignored)

  final def observe[T <: Entity](request : Request[T, _]) : ObservableAccessSet[Relation, T] =
    observe(request, Set[SVarActor.Ref]())

  final def observe[T <: Entity](r : RightRelationPart[_ >: SelfType , T]) : ObservableAccessSet[Relation, Relation] =
    observe(r, Set[SVarActor.Ref]())

  final def observe[T <: Entity](request : Request[T, _], ignored : Set[SVarActor.Ref]) : ObservableAccessSet[Relation, T] =
    _observe[T](request.description, ignored, request.accessValue )

  final def observe[T <: Entity](r : RightRelationPart[_ >: SelfType  , T], ignored : Set[SVarActor.Ref]) : ObservableAccessSet[Relation, Relation] =
    _observe[Relation](r.complete(asSelfType).description, ignored, x => x)


  // The real access methods
  private def _observe[T](desc : RelationDescription[_, _], ignored : Set[SVarActor.Ref], convert : Relation => T) =
    new ObservableAccessSet[Relation, T](this, access(desc, _, filter(desc, _)), _.observe(ignored), convert)

  private def _get[T](desc : RelationDescription[_, _], convert : Relation => T,
                      at : Time, accessMethod : AccessMethod) : AccessSet[Relation, T] =
    new AccessSet[Relation, T](access(desc, _, filter(desc, _)), _.accessValue(at, accessMethod), convert)

  private def filter(desc : RelationDescription[_, _], stateParticle : StateParticle[Relation]) : Boolean = {
    stateParticle.getValue.collect{ case relation =>
      relation.getSubject.description.typeDef.isSubtypeOf(desc.leftDesc) &&
        relation.getObject.description.typeDef.isSubtypeOf(desc.rightDesc)
    }.getOrElse(true)
  }

  // remove
  def remove[T](c : ConvertibleTrait[T], handler : SelfType => Any = _ => {})(implicit actor : EntityUpdateHandling)
}
