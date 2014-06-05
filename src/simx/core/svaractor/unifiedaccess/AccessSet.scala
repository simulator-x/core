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

import simx.core.svaractor.StateParticle
import scala.language.higherKinds
import simx.core.ontology

/**
 * Created by dwiebusch on 04.03.14
 * Set for unified Access
 */

private object AccessSet{
  type AccessFunc[T] =  StateParticle[T] => ((T => Unit, EntityUpdateHandling) => Unit)
}

sealed class AccessSet[T, U] protected[unifiedaccess]( accessValues : EntityUpdateHandling => (AnnotatedMap[T] => Any) => Unit,
                                                       accessSVars  : AccessSet.AccessFunc[T], convert : T => U)
{
  protected type TupleWithParticle[X] = (MapKey[X], StateParticle[X])
  private type TupleWithValue[X]    = (AnnotationSet, X)
  private type Identity[X]          = X

  protected def internalObserve[MapEntry[_], Handlable[_], X]( iterate : (AnnotatedMap[T], MapEntry[T] => Any) => Unit,
                                                               access  : (MapEntry[T], AccessSet.AccessFunc[T], Handlable[X] => Any) => Unit,
                                                               handle  : Handlable[X] => Any,
                                                               filter  : AnnotatedMap[T] => AnnotatedMap[T] = m => m)
                                                             ( implicit svarActor : EntityUpdateHandling )  : Option[java.util.UUID] =
  {
    accessValues(svarActor){ map => iterate(filter(map), entry => access(entry, accessSVars, handle)) }
    None
  }


  final protected def _access[M[_]](iterate : (AnnotatedMap[T], TupleWithParticle[T] => Any) => Unit,
                                    handler : M[U] => Any,
                                    f : TupleWithValue[U] => M[U],
                                    svarActor : EntityUpdateHandling) =
    internalObserve[TupleWithParticle, M, U](iterate,
      (tuple, access, handle) => access(tuple._2)(value => handle(f(tuple._1.set, convert(value))), svarActor),
      handler
    )(svarActor)

  final def apply(handler : (AnnotationSet, U) => Any)(implicit svarActor : EntityUpdateHandling) =
    foreach(handler)(svarActor)

  final def apply(handler : U => Any)(implicit svarActor : EntityUpdateHandling) =
    foreach(handler)(svarActor)

  def ifEmpty(handler :  =>  Any)(implicit svarActor : EntityUpdateHandling) =
    new AccessSet[T, U](svarActor => h => accessValues(svarActor).apply(map => if (map.isEmpty) handler else h(map)),
      accessSVars, convert)

  private def _head[M[_]](handler : M[U] => Any, f : TupleWithValue[U] => M[U], svarActor : EntityUpdateHandling) =
    _access((map, access) => map.headOption.collect { case defined => access(defined) }, handler, f, svarActor)

  @deprecated("use head instead", "")
  final def first(handler : (AnnotationSet, U) => Any)(implicit svarActor : EntityUpdateHandling) =
    _head[TupleWithValue](x => handler(x._1, x._2), y => y, svarActor)

  @deprecated("use head instead", "")
  final def first(handler : U => Any)(implicit svarActor : EntityUpdateHandling) =
    _head[Identity](handler, _._2, svarActor)

  final def head(handler : (AnnotationSet, U) => Any)(implicit svarActor : EntityUpdateHandling) =
    _head[TupleWithValue](x => handler(x._1, x._2), y => y, svarActor)

  final def head(handler : U => Any)(implicit svarActor : EntityUpdateHandling) =
    _head[Identity](handler, _._2, svarActor)

  private def _foreach[M[_]](handler : M[U] => Any, f : TupleWithValue[U] => M[U], svarActor : EntityUpdateHandling) =
    _access((map, access) => map.foreach(access), handler, f, svarActor)

  final def foreach(handler : (AnnotationSet, U) => Any)(implicit svarActor : EntityUpdateHandling) =
    _foreach[TupleWithValue]( x => handler(x._1, x._2), y => y, svarActor)

  final def foreach(handler : U => Any)(implicit svarActor : EntityUpdateHandling) =
    _foreach[Identity]( handler, _._2, svarActor)

  final def forall(handler : AnnotatedMap[T] => Any)(implicit svarActor : EntityUpdateHandling) =
    internalObserve[AnnotatedMap, AnnotatedMap, T](
      (map, handle) => handle(map),
      (map, _, handle) => handle(map),
      handler
    )
}

final class ObservableAccessSet[T, U] protected[unifiedaccess]( entity       : StateParticleAccess,
                                                                accessValues : EntityUpdateHandling  => (AnnotatedMap[T] => Any) => Unit,
                                                                accessFunc   : AccessSet.AccessFunc[T],
                                                                convert      : T => U)
  extends AccessSet(accessValues, accessFunc, convert)
{
  override protected def internalObserve[MapEntry[_], Handlable[_], X]( iterate : (AnnotatedMap[T], MapEntry[T] => Any) => Unit,
                                                                        access  : (MapEntry[T], AccessSet.AccessFunc[T], Handlable[X] => Any) => Unit,
                                                                        handle  : Handlable[X] => Any,
                                                                        filter  : AnnotatedMap[T] => AnnotatedMap[T] = m => m)
                                                                      ( implicit svarActor : EntityUpdateHandling ) : Option[java.util.UUID] =
  {
    super.internalObserve(iterate, access, handle)
    var knownSVars = entity.getAllStateParticles(svarActor).map(_._3).toSet
    Some(entity.onUpdate{ newEntity =>
      super.internalObserve(iterate, access, handle, _.filterNot( knownSVars contains _._2 ))
      accessValues(svarActor) {
        map => iterate(map.filterNot( knownSVars contains _._2 ), entry => access(entry, _.accessValue, handle))
      }
      knownSVars = newEntity.getAllStateParticles(svarActor).map(_._3).toSet
    })
  }

  override def ifEmpty(handler : => Any)(implicit svarActor: EntityUpdateHandling): ObservableAccessSet[T, U] =
    new ObservableAccessSet[T, U](entity, actor => h => accessValues(actor)(map => if (map.isEmpty) handler else h(map)),
      accessFunc, convert)

  def onChange(func : PartialFunction[Change, Unit])(implicit svarActor : EntityUpdateHandling) {
    accessValues(svarActor) {
      initialMap =>
        var knownSVars = initialMap
        knownSVars.foreach { x =>
          x._2.get(value => func(Add(x._1.set.annotations.toSet, value)))
          x._2.observe(value => func(Update(x._1.set.annotations.toSet, value)))
        }
        val updateId = entity.onUpdate { newEntity =>
          accessValues(svarActor) {
            updatedMap =>
              val newSVars = updatedMap.filterNot(newSVar => knownSVars.exists(_._2 == newSVar._2))
              newSVars.filter(newSVar => updatedMap.exists(_._2 == newSVar._2)).foreach {
                newTriple =>
                  newTriple._2.get(value => func(Add(newTriple._1.set.annotations.toSet, value)))
                  newTriple._2.observe(value => func(Update(newTriple._1.set.annotations.toSet, value)))
              }
              val removedSVars = knownSVars.filterNot(knownSVar => updatedMap.exists(_._2 == knownSVar._2))
              knownSVars.foreach(x => func(Remove(x._1.set.annotations.toSet)))
              knownSVars = (knownSVars ++ newSVars) -- removedSVars.keys
          }
        }
        svarActor.storeUpdateId(updateId, this)
    }
  }

  def ignore()(implicit svarActor : EntityUpdateHandling){
    svarActor.getUpdateId(this).collect{ case id => entity.ignore(id) }
  }
}

sealed abstract class Change(val annotations : Set[ontology.Annotation])
case class Add[T](override val annotations : Set[ontology.Annotation], value : T) extends Change(annotations)
case class Update[T](override val annotations : Set[ontology.Annotation], value : T) extends Change(annotations)
case class Remove[T](override val annotations : Set[ontology.Annotation]) extends Change(annotations)
