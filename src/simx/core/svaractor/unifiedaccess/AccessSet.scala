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
import simx.core.svaractor.TimedRingBuffer.{Time, ContentType}
import scala.language.higherKinds
import simx.core.ontology

/**
 * Created by dwiebusch on 04.03.14
 * Set for unified Access
 */

private object AccessSet{
  type AccessFunc[T] =  StateParticle[T] => ((ContentType[T] => Unit, EntityUpdateHandling) => Unit)
  type ObserveFunc[T] =  StateParticle[T] => ((T => Unit, EntityUpdateHandling) => Unit)
}

/**
 *
 *  * @param entity the entity which shall be observed
 * @param accessValues the method that is used to access the state particles in the given entity, taking another function
 *                   as a parameter which then operates on the accessed values (which usually is the svars get method)
 * @param convert an conversion function that converts the state particles types into a desired representation
 *                (in most cases this will simply return its input)
 * @tparam T the type of the accessed state particles
 * @tparam U the type into which the state particles content will be converted before handlers do operate on it (often U = T)
 */
sealed class AccessSet[T, U] protected[unifiedaccess]( accessValues : EntityUpdateHandling => (AnnotatedMap[T] => Any) => Unit,
                                                       accessSVars  : AccessSet.AccessFunc[T],
                                                       convert : T => U)
{
  protected type TupleWithParticle[X] = (MapKey[X], StateParticle[X])
  private type TupleWithValue[X]      = (AnnotationSet, ContentType[X])
  private type Identity[X]            = ContentType[X]

  /**
   * the method combining the provided methods to actually access the state particles of the given entity
   * @param iterate the method which iterates over the accessed state particles and their annotations (which,
   *                in the case of the head method, only selects the first value), filtered using the filter function first
   * @param access the state particle's method that is called when iterating over the state particles (e.g. get)
   * @param handle the handler which is executed in the end (which is passed to the get or observe method of the state particle)
   * @param filter an optional filter function that filters state particles before other functions are applied
   * @param svarActor the actor context in which the access is executed
   * @tparam MapEntry the value type of the map resulting from a call to access (either a tuple or a single value)
   * @tparam Handlable the type of the state particle
   * @tparam X the data type of the state particle (after an eventual conversion)
   * @return None
   */
  protected def internalObserve[MapEntry[_], Handlable[_], X]( iterate : (AnnotatedMap[T], MapEntry[T] => Any) => Unit,
                                                               access  : (MapEntry[T], AccessSet.AccessFunc[T], Handlable[X] => Any) => Unit,
                                                               handle  : Handlable[X] => Any,
                                                               filter  : AnnotatedMap[T] => AnnotatedMap[T] = m => m)
                                                             ( implicit svarActor : EntityUpdateHandling )  : Option[java.util.UUID] =
  {
    accessValues(svarActor){ map => iterate(filter(map), entry => access(entry, accessSVars, handle)) }
    None
  }


  /**
   * a wrapper fo the internal observe method
   * @param iterate the method which iterates over the accessed state particles and their annotations (which,
   *                in the case of the head method, only selects the first value), filtered using the filter function first
   * @param handler the handler which is executed in the end (which is passed to the get or observe method of the state particle)
   * @param f a conversion function that converts an entry of the accesses map into the desired format
   *          (M[U] which may be U when using the Identity type defined above)
   * @param svarActor the actor context in which the access is executed
   * @tparam M the value type of the map resulting from a call to access (either a tuple or a single value)
   * @return None
   */
  final protected def _access[M[_]](iterate : (AnnotatedMap[T], TupleWithParticle[T] => Any) => Unit,
                                    handler : M[U] => Any,
                                    f : TupleWithValue[U] => M[U],
                                    svarActor : EntityUpdateHandling) =
    internalObserve[TupleWithParticle, M, U](iterate,
      (tuple, access, handle) => access(tuple._2)(value => handle(f(tuple._1.set, (convert(value._1), value._2))), svarActor),
      handler
    )(svarActor)

  /**
   * a wrapper for the foreach function
   * @param handler the handler to be executed on the accessed state particles and their annotations
   * @param svarActor the actor context in which the handler is executed
   * @return None
   */
  final def apply(handler : (AnnotationSet, U, Time) => Any)(implicit svarActor : EntityUpdateHandling) =
    foreach(handler)(svarActor)

  /**
   * a wrapper for the foreach function
   * @param handler the handler to be executed on the accessed state particles
   * @param svarActor the actor context in which the handler is executed
   * @return None
   */
  final def apply(handler : (U, Time) => Any)(implicit svarActor : EntityUpdateHandling) =
    foreach(handler)(svarActor)

  final def apply(handler : U => Any)(implicit svarActor : EntityUpdateHandling) =
    foreach(handler)(svarActor)

  /**
   * @param handler the handler to be executed there are no results for the requested state particles
   * @param svarActor the actor context in which the handler will be executed
   * @return a new representation of "this" having integrated the given handler
   */
  def ifEmpty(handler :  =>  Any)(implicit svarActor : EntityUpdateHandling) =
    new AccessSet[T, U](svarActor => h => accessValues(svarActor).apply(map => if (map.isEmpty) handler else h(map)),
      accessSVars, convert)

  private def _head[M[_]](handler : M[U] => Any, f : TupleWithValue[U] => M[U], svarActor : EntityUpdateHandling) =
    _access((map, access) => map.headOption.collect { case defined => access(defined) }, handler, f, svarActor)

  @deprecated("use head instead", "")
  final def first(handler : (AnnotationSet, U, Time) => Any)(implicit svarActor : EntityUpdateHandling) =
    head(handler)

  @deprecated("use head instead", "")
  final def first(handler : (U, Time) => Any)(implicit svarActor : EntityUpdateHandling) =
    head(handler)

  @deprecated("use head instead", "")
  final def first(handler : U => Any)(implicit svarActor : EntityUpdateHandling) =
    head(handler)

  /**
   * @param handler the handler which is executed after accessing the first state particle of the accessed ones and its annotations
   * @param svarActor the actor context in which the handler will be executed
   * @return None
   */
  final def head(handler : (AnnotationSet, U, Time) => Any)(implicit svarActor : EntityUpdateHandling) =
    _head[TupleWithValue](x => handler(x._1, x._2._1, x._2._2), y => y, svarActor)

  /**
   * @param handler the handler which is executed after accessing the first state particle of the accessed ones
   * @param svarActor the actor context in which the handler will be executed
   * @return None
   */
  final def head(handler : (U, Time) => Any)(implicit svarActor : EntityUpdateHandling) =
    _head[Identity](x => handler(x._1, x._2), _._2, svarActor)

  final def head(handler : U => Any)(implicit svarActor : EntityUpdateHandling) =
    _head[Identity](x => handler(x._1), _._2, svarActor)

  private def _foreach[M[_]](handler : M[U] => Any, f : TupleWithValue[U] => M[U], svarActor : EntityUpdateHandling) =
    _access((map, access) => map.foreach(access), handler, f, svarActor)

  /**
   * @param handler the handler which is executed on all accessed state particles and their annotations
   * @param svarActor the actor context in which the handler will be executed
   * @return None
   */
  final def foreach(handler : (AnnotationSet, U, Time) => Any)(implicit svarActor : EntityUpdateHandling) =
    _foreach[TupleWithValue]( x => handler(x._1, x._2._1, x._2._2), y => y, svarActor)

  /**
   * @param handler the handler which is executed on all accessed state particles
   * @param svarActor the actor context in which the handler will be executed
   * @return None
   */
  final def foreach(handler : (U, Time) => Any)(implicit svarActor : EntityUpdateHandling) =
    _foreach[Identity]( x => handler(x._1, x._2), _._2, svarActor)

  final def foreach(handler : U => Any)(implicit svarActor : EntityUpdateHandling) =
    _foreach[Identity]( x => handler(x._1), _._2, svarActor)

  /**
   * @param handler the handler which operates on the accessed state particles
   * @param svarActor the actor context in which the handler will be executed
   * @return None
   */
  final def forall(handler : AnnotatedMap[T] => Any)(implicit svarActor : EntityUpdateHandling) =
    internalObserve[AnnotatedMap, AnnotatedMap, T](
      (map, handle) => handle(map),
      (map, _, handle) => handle(map),
      handler
    )
}

/**
 *
 * @param entity the entity which shall be observed
 * @param accessValues the method that is used to access the state particles in the given entity, taking another function
 *                   as a parameter which then operates on the accessed values (which usually is the svars observe method)
 * @param accessFunc the method which is called on the extracted state particle (probably observe)
 * @param convert an conversion function that converts the state particles types into a desired representation
 *                (in most cases this will simply return its input)
 * @tparam T the type of the accessed state particles
 * @tparam U the type into which the state particles content will be converted before handlers do operate on it (often U = T)
 */

final class ObservableAccessSet[T, U] protected[unifiedaccess]( entity       : StateParticleAccess,
                                                                accessValues : EntityUpdateHandling  => (AnnotatedMap[T] => Any) => Unit,
                                                                accessFunc   : AccessSet.AccessFunc[T],
                                                                convert      : T => U)
  extends AccessSet(accessValues, accessFunc, convert)
{

  /**
   * the method combining the provided methods to actually access the state particles of the given entity. In addition
   * to the super classes implementation, it also observes changes to the entity and updates its behavior accordingly
   * @param iterate the method which iterates over the accessed state particles and their annotations (which,
   *                in the case of the head method, only selects the first value), filtered using the filter function first
   * @param access the state particle's method that is called when iterating over the state particles (e.g. get)
   * @param handle the handler which is executed in the end (which is passed to the get or observe method of the state particle)
   * @param filter an optional filter function that filters state particles before other functions are applied
   * @param svarActor the actor context in which the access is executed
   * @tparam MapEntry the value type of the map resulting from a call to access (either a tuple or a single value)
   * @tparam Handlable the type of the state particle
   * @tparam X the data type of the state particle (after an eventual conversion)
   * @return an Option containing the id which can be used to abort observing the entity using ignore
   */
  override protected def internalObserve[MapEntry[_], Handlable[_], X]( iterate : (AnnotatedMap[T], MapEntry[T] => Any) => Unit,
                                                                        access  : (MapEntry[T], AccessSet.AccessFunc[T], Handlable[X] => Any) => Unit,
                                                                        handle  : Handlable[X] => Any,
                                                                        filter  : AnnotatedMap[T] => AnnotatedMap[T] = m => m)
                                                                      ( implicit svarActor : EntityUpdateHandling ) : Option[java.util.UUID] =
  {
    val knownSVars = collection.mutable.Set[StateParticle[_]]()
    accessValues(svarActor)(knownSVars ++= _.values)
    super.internalObserve(iterate, access, handle)
    Some(entity.onUpdate{ newEntity =>
      super.internalObserve(iterate, access, handle, _.filterNot( knownSVars contains _._2) )
      knownSVars ++= newEntity.getAllStateParticles(svarActor).map(_.svar)
    })
  }

  /**
   * @param handler the handler to be executed there are no results for the requested state particles
   * @param svarActor the actor context in which the handler will be executed
   * @return a new representation of "this" having integrated the given handler
   */
  override def ifEmpty(handler : => Any)(implicit svarActor: EntityUpdateHandling): ObservableAccessSet[T, U] =
    new ObservableAccessSet[T, U](entity, actor => h => accessValues(actor)(map => if (map.isEmpty) handler else h(map)),
      accessFunc, convert)

  /**
   * @param func a handler that is executed each time one of the requested state particles changes (i.e. is added, updated, or removed)
   * @param svarActor the actor context in which the handler is executed
   */
  def onChange(func : PartialFunction[Change[U], Unit])(implicit svarActor : EntityUpdateHandling) {
    val knownValues = collection.mutable.WeakHashMap[StateParticle[T], U]()

    def recognizeNewVal(stateParticle : StateParticle[T], value : Option[ContentType[T]], annotations : AnnotationSet,
                        isRemove : Boolean) {
      if (isRemove){
        val myValue = knownValues.remove(stateParticle)
        if (myValue.isDefined)
          func(Remove(annotations.annotations.toSet, myValue.get))
      }
      else if (knownValues.contains(stateParticle)) {
        val converted = convert(value.get._1)
        knownValues.update(stateParticle, converted)
        func(Update(annotations.annotations.toSet, converted))
      } else {
        val converted = convert(value.get._1)
        knownValues.update(stateParticle, converted)
        func(Add(annotations.annotations.toSet, convert(value.get._1)))
      }
    }

    accessValues(svarActor) {
      initialMap =>
        var knownSVars = initialMap
        knownSVars.foreach { x =>
          x._2.observe((value, time) => recognizeNewVal(x._2, Some(value -> time), x._1.set, isRemove = false))
        }
        val updateId = entity.onUpdate { newEntity =>
          accessValues(svarActor) {
            updatedMap =>
              val newSVars = updatedMap.filterNot(newSVar => knownSVars.exists(_._2 == newSVar._2))
              newSVars.filter(newSVar => updatedMap.exists(_._2 == newSVar._2)).foreach { newTriple =>
                newTriple._2.observe((value, time) => recognizeNewVal(newTriple._2, Some(value -> time), newTriple._1.set, isRemove = false))
              }
              val removedSVars = knownSVars.filterNot(knownSVar => updatedMap.exists(_._2 == knownSVar._2))
              removedSVars.foreach(x => recognizeNewVal(x._2, None, x._1.set, isRemove = true))
              knownSVars = (knownSVars ++ newSVars) -- removedSVars.keys
          }
        }
        svarActor.storeUpdateId(updateId, this)
    }
  }

  /**
   * stops the registered onChange function
   * @param svarActor the actor context for which the updates will be stopped
   */
  def ignore()(implicit svarActor : EntityUpdateHandling){
    svarActor.getUpdateId(this).collect{ case id => entity.ignore(id) }
  }
}

sealed abstract class Change[T](val annotations : Set[ontology.Annotation], value : Option[T])
case class Add[T](override val annotations : Set[ontology.Annotation], value : T) extends Change[T](annotations, Some(value))
case class Update[T](override val annotations : Set[ontology.Annotation], value : T) extends Change[T](annotations, Some(value))
case class Remove[T](override val annotations : Set[ontology.Annotation], value : T) extends Change[T](annotations, Some(value))
