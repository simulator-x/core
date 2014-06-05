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

package simx.core.helper

import collection.mutable.ListBuffer


/**
 * @author dwiebusch
 * Date: 23.10.11
 * Time: 17:17
 */

object Execute{
  def inParallel[T](func : (T => Unit) => Unit) =
    ExecInParallel[T, Unit]( func, None, () => {} )

  def allInParallel[T]( tasks : Seq[(T => Unit) => Unit] ) : ExecInParallel[T, _] =
    inParallel(tasks.head) andAllOf tasks.tail

  @deprecated("check this, seems to yield wrong results", "")
  def allInParallel2[T](tasks : Seq[(T => Unit) => Unit] ) =
    tasks.tail.foldLeft(ExecInParallel2(tasks.head))(_ and _)

  def serialized[T](func : (T => Unit) => Unit) =
    ExecSerialized[T, Unit](func, () => {})

  def allSerialized[T](tasks : Seq[((T => Unit) => Unit)]) : ExecSerialized[T, _] =
    serialized(tasks.head) andAllOf tasks.tail

  def allSerialized2[T](tasks : Seq[((T => Unit) => Unit)]) =
    tasks.tail.foldLeft(ExecSerialized2(tasks.head))(_ and _)
}

abstract class Execute[T, U]{
  protected val prevVal : () => U
  protected var result  : Option[T] = None
  protected def get     : (U,  T)   = (prevVal(), result.get)
  protected def set(v : T) { result = Some(v) }
  def exec( x : ((U, T)) => Unit )
  def exec(){ exec( x => {} ) }
}

trait ParallelExecution{
  private val counter = new java.util.concurrent.atomic.AtomicInteger(0)
  protected val parent : Option[ParallelExecution] = None
  protected def increment() : Int = parent.collect{ case p => p.increment() }.getOrElse(counter.incrementAndGet())
  protected def decrement() : Int = parent.collect{ case p => p.decrement() }.getOrElse(counter.decrementAndGet())
}

case class ExecInParallel2[T](func : (T => Unit) => Unit){
  private val counter = new java.util.concurrent.atomic.AtomicInteger(1)
  private case class Task(f : (T => Unit) => Unit){
    private var result : Option[T] = None
    def start(handler : List[T] => Any){
      func{t =>
        result = Some(t)
        if (counter.decrementAndGet() == 0)
          handler(parallelTasks.toList.map(_.result.get))
      }
    }
  }
  private val parallelTasks = ListBuffer(Task(func))
  def and( otherFunc : (T => Unit) => Unit) ={
    counter.incrementAndGet()
    parallelTasks.append(Task(otherFunc))
    this
  }

  def exec( handler : List[T] => Any){
    println(parallelTasks.size + " " + counter.get())
    parallelTasks.foreach(_.start(handler))
  }
}

case class ExecSerialized2[T](func : (T => Unit) => Unit){
  private case class Task(f : (T => Unit) => Unit ){
    def start( next : List[Task], handler : List[T] => Any, results : List[T] = Nil ){
      f.apply( result => next match{
        case head :: tail => head.start(tail, handler, result :: results)
        case Nil => handler((result :: results).reverse)
      })
    }
  }

  private val tasks = ListBuffer(Task(func))

  def and( otherFunc : (T => Unit) => Unit) = {
    tasks.append(Task(otherFunc))
    this
  }

  def exec( handler : List[T] => Any ) {
    tasks.headOption.collect{ case head => head.start(tasks.tail.toList, handler) }
  }
}

case class ExecInParallel[T, U] protected[helper]( func : (T => Unit) => Unit,
                                                   override val parent : Option[ExecInParallel[_, _]],
                                                   prevVal : () => U) extends Execute[T,  U] with ParallelExecution {
  increment()

  private def set( handler : () => Unit )(value : T) {
    set(value)
    if (decrement() == 0)
      handler()
  }

  private def exec( handler: () => Unit ){
    parent.collect{ case m => m.exec(handler) }
    func(set(handler))
  }

  def and[W](that : (W => Unit) => Unit) =
    ExecInParallel( that, Some(this), () => get )

  def andAllOf[W <: T]( tasks : Seq[(W => Unit) => Unit]) : ExecInParallel[T, _] = {
    var retVal : ExecInParallel[T, _] = this
    for (task <- tasks) retVal = retVal and task
    retVal
  }

  def andThen[V]( nextFunc : (V => Unit) => Unit ) : ExecSerialized[V,  (U, T)] = {
    var collected : Option[(U, T)] = None
    ExecSerialized( (x : V => Unit ) => exec(t => {collected = Some(t); nextFunc(x)}), () => collected.get )
  }

  def andThen[V]( next : ExecSerialized[V,  _] ) : ExecSerialized[V,  (U, T)] =
    andThen(next.func)

  def exec( x : ((U, T)) => Unit ){
    exec( () => x(get) )
  }
}

case class ExecSerialized[T, U](func : (T => Unit) => Unit, prevVal : () => U) extends Execute[T,  U]{
  def andThen[V](f : ((U, T)) => (V => Unit) => Unit) =
    ExecSerialized[V, (U, T)]( (v : V => Unit) => func(t => { set(t); f(get)(v)} ), () => get )

  def and[V]( f : (V => Unit) => Unit) : ExecSerialized[V, (U,  T)] =
    ExecSerialized( (v : V => Unit) => func(t => {set(t); f(v) }), () => get)

  def andAllOf[X <: T](tasks : Seq[((X => Unit) => Unit)]) : ExecSerialized[T, _] = {
    var retVal : ExecSerialized[T,  _] = this
    for (task <- tasks) retVal = retVal and task
    retVal
  }

  def exec(v : ((U, T)) => Unit) {
    func( t => {set(t); v(get)} )
  }
}

case class NewExecScheme[T]( handler : (T => Any) => Unit, override val parent : Option[NewExecScheme[T]] = None )
  extends ParallelExecution
{
  private var result  : Option[T] = None
  increment()

  private def set( finalHandler : () => Any, value : T ){
    result = Some(value)
    if (decrement() == 0)
      finalHandler()
  }

  protected def getValues : Set[T] =
    Set(result.get) ++ parent.collect{ case p => p.getValues }.getOrElse(Set())

  protected def exec(finalHandler : () => Any){
    parent.collect{ case p => p.exec(finalHandler) }
    handler{ value => set(finalHandler, value) }
  }

  def and[U <: T]( handler2 : (U => Any) => Unit ) =
    NewExecScheme( handler2, Some(this) )

  def exec( finalHandler : Set[T] => Any ){
    exec{() => finalHandler(getValues) }
  }
}

object Beautification{
  def beautify[A, B, C, D, E, F, G, H, I, J, K]( input : ((((((((((A, B), C), D), E), F), G), H), I), J), K) ) : (A, B, C, D, E, F, G, H, I, J, K) =
    (input._1._1._1._1._1._1._1._1._1._1, input._1._1._1._1._1._1._1._1._1._2, input._1._1._1._1._1._1._1._1._2, input._1._1._1._1._1._1._1._2,
      input._1._1._1._1._1._1._2, input._1._1._1._1._1._2, input._1._1._1._1._2, input._1._1._1._2, input._1._1._2, input._1._2, input._2)

  def beautify[A, B, C, D, E, F, G, H, I, J]( input : (((((((((A, B), C), D), E), F), G), H), I), J) ) : (A, B, C, D, E, F, G, H, I, J) =
    (input._1._1._1._1._1._1._1._1._1, input._1._1._1._1._1._1._1._1._2, input._1._1._1._1._1._1._1._2, input._1._1._1._1._1._1._2,
      input._1._1._1._1._1._2, input._1._1._1._1._2, input._1._1._1._2, input._1._1._2, input._1._2, input._2)

  def beautify[A, B, C, D, E, F, G, H, I]( input : ((((((((A, B), C), D), E), F), G), H), I) ) : (A, B, C, D, E, F, G, H, I) =
    (input._1._1._1._1._1._1._1._1, input._1._1._1._1._1._1._1._2, input._1._1._1._1._1._1._2, input._1._1._1._1._1._2, input._1._1._1._1._2,
      input._1._1._1._2, input._1._1._2, input._1._2, input._2)

  def beautify[A, B, C, D, E, F, G, H]( input : (((((((A, B), C), D), E), F), G), H) ) : (A, B, C, D, E, F, G, H) =
    (input._1._1._1._1._1._1._1, input._1._1._1._1._1._1._2, input._1._1._1._1._1._2, input._1._1._1._1._2,
      input._1._1._1._2, input._1._1._2, input._1._2, input._2)

  def beautify[A, B, C, D, E, F, G]( input : ((((((A, B), C), D), E), F), G) ) : (A, B, C, D, E, F, G) =
    (input._1._1._1._1._1._1, input._1._1._1._1._1._2, input._1._1._1._1._2, input._1._1._1._2, input._1._1._2, input._1._2, input._2)

  def beautify[A, B, C, D, E, F]( input : (((((A, B), C), D), E), F) ) : (A, B, C, D, E, F) =
    (input._1._1._1._1._1, input._1._1._1._1._2, input._1._1._1._2, input._1._1._2, input._1._2, input._2)

  def beautify[A, B, C, D, E]( input : ((((A, B), C), D), E) ) : (A, B, C, D, E) =
    (input._1._1._1._1, input._1._1._1._2, input._1._1._2, input._1._2, input._2)

  def beautify[A, B, C, D]( input : (((A, B), C), D) ) : (A, B, C, D) =
    (input._1._1._1, input._1._1._2, input._1._2, input._2)

  def beautify[A, B, C]( input : ((A, B), C) ) : (A, B, C) =
    (input._1._1, input._1._2, input._2)
}
