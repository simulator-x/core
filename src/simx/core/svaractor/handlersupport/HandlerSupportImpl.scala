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

package simx.core.svaractor.handlersupport

import simx.core.svaractor.handlersupport.Types.CPSRet

import scala.collection.mutable
import java.util.UUID
import scala.reflect.{classTag, ClassTag}
import scala.util.continuations._

/* author: dwiebusch
 * date: 19.09.2010
 */

case class UnhandledMessage private[svaractor](msg : Any, reason : String = "")

trait HandlerSupportImpl extends HandlerSupport{
  private type IdHandlerPair   = (IdType, Function[Any, Any])
  private type handlerList     = List[IdHandlerPair]

  private var lastId : IdType  = 0
  private val handlers         = mutable.Map[Class[_], handlerList]()

  var messageProcessingLog = Map[Class[_],List[(UUID,Long,Long,Long)]]()

  protected val neverMatchingHandler = new handler_t {
    def isDefinedAt(x: Any) = false
    def apply(v1: Any) {}
  }

  protected val handlersAsPF = new PartialFunction[Any, Any] {
    def apply(x: Any) = { applyHandlers(x) }
    def isDefinedAt(x: Any) = true
  }

  protected def addHandler[T : ClassTag](handler: Function[T, Any@CPSRet]){
    handler match{
      case pf : tmpType @unchecked => updateHandlers( classTag[T].runtimeClass, (generateId(),
        new PartialFunction[Any, Any] {
          override def toString() = "Handler for type " + classTag[T]
          override def isDefinedAt(x: Any): Boolean = pf.isDefinedAt(x)
          override def apply(a: Any): Any = reset[Any, Any] {
            val ret = handler(a.asInstanceOf[T])
            shift { (x: Unit => Any) => ret }
          }
        }))
      case f => updateHandlers( classTag[T].runtimeClass, (generateId(), {
        (msg : Any) => reset[Any, Any]{
          val ret = handler(msg.asInstanceOf[T])
          shift{(x : Unit => Any) => ret }
        }
      }))
    }
  }

  protected def addHandlerPF[T : ClassTag]( pf: PartialFunction[T, Any@CPSRet] ){
    addHandler[T](pf)
  }

  protected def removeHandler( id: IdType, manifest : Class[_])  {
    handlers.update(manifest, handlers.getOrElse(manifest, Nil).filterNot( _._1 == id ) )
  }

  protected def addSingleUseHandler[T : ClassTag]( f: handlerType[T] ) {
    addSingleUseHandler(wrap(f), classTag[T].runtimeClass)
  }

  protected def addSingleUseHandlerPF[T : ClassTag]( pf: PartialFunction[T, Any@CPSRet] ) {
    addSingleUseHandler(pf.asInstanceOf[PartialFunction[Any, Any@CPSRet]], classTag[T].runtimeClass)
  }

  protected def addSingleUseHandlerPF2[T : ClassTag](id : java.util.UUID, msg: Any, pf: PartialFunction[T, Any@CPSRet] ) {
    print(this + " registering " + id + " for msg " + msg)
    addSingleUseHandler(pf.asInstanceOf[PartialFunction[Any, Any@CPSRet]], classTag[T].runtimeClass)
  }


  protected def applyHandlers(msg : Any) : Any = {
    val msgManifest = msg.getClass
    handlers get msgManifest match{
      case Some(list) =>
        applyHandlerList( list, msg )
      case None =>
        for ( ( manifest, list ) <- handlers )
          if ( manifest isAssignableFrom msgManifest )
            return applyHandlerList( list, msg )
        UnhandledMessage(msg, "no matching handler found")
    }
  }

  protected[svaractor] def addSingleUseHandler[T](pf: handlerC_t, manifest : Class[_]) {
    updateHandlers(manifest, toRemovableTuple(pf, manifest, generateId() ), append = true )
  }

  private[svaractor] def toRemovableTuple(pf : handlerC_t, manifest : Class[_], id : IdType) = id -> new PartialFunction[Any, Any] {
    override def toString() = pf.toString()
    override def isDefinedAt(x: Any) = pf.isDefinedAt(x)
    override def apply(v1: Any) = {
      reset[Any, Any] {
        val ret = pf(v1)
        removeHandler(id, manifest)
        shift { (x: Unit => Any) => ret }
      }
    }
  }

  /*
   This can cause severe errors, if called from another actor's context
   */
  private[svaractor] def generateId() : IdType =
    (lastId += 1, lastId)._2

  private[svaractor] def updateHandlers( manifest : Class[_], toAdd : IdHandlerPair, append : Boolean = false ) {
    handlers.update(manifest, if (append) handlers.getOrElse(manifest, Nil) :+ toAdd else toAdd :: handlers.getOrElse(manifest, Nil) )
  }

  //  private[svaractor] def continueWith[A, T]( f : Function[A, T])(implicit manifest : ClassTag[A]) : T @HandlerContinuation =
  //    shift { (fun : T => HandlerOption) => Some( wrap(f) andThen fun andThen storeContinuation, manifest ) }

  private type tmpType = PartialFunction[Any, _]

  private[handlersupport] def applyHandlerList( list : handlerList, msg : Any ) : Any = {
    if (list.isEmpty) return UnhandledMessage(msg, "no existing handler") else list.foreach {
      case (_, pf: tmpType) => if (pf isDefinedAt msg) return pf(msg)
      case (_, f) => return f(msg)
    }
    UnhandledMessage(msg, "no matching handler in " + list.size + " available handlers:\n\t" + list.mkString("\n\t"))
  }

  private def wrap[T, U](func : Function[T, U@CPSRet])(implicit manifest : ClassTag[T]) : PartialFunction[Any, U@CPSRet] = func match {
    case pf : PartialFunction[_, U@CPSRet@unchecked] => pf.asInstanceOf[PartialFunction[Any, U@CPSRet]]
    case f => new PartialFunction[Any, U@CPSRet] {
      override def toString() = "Wrapped handler for type " + manifest
      def isDefinedAt(x: Any) = manifest.runtimeClass isAssignableFrom x.getClass
      def apply(v1: Any) = f.apply(v1.asInstanceOf[T])
    }
  }

  // CPS helpers:
  protected def msgMatch[T](partialFunction: Function[T, Any])(msg : T) : Any@CPSRet =
    partialFunction(msg)

  protected def Match[T](msg : T)(partialFunction: PartialFunction[T, Any]) : Any@CPSRet  =
    partialFunction(msg)

  def If(condition: => scala.Boolean@CPSRet): If@CPSRet =
    new If(condition)
}

protected[core] class If (condition : => scala.Boolean@CPSRet){
  protected abstract class Else[+T]{self =>
    protected def createHalt[U >: T](func: U) : Else[U]@CPSRet
    def ElseIf(condition : => scala.Boolean@CPSRet) : Then@CPSRet
    def Else[U >: T](func : => U@CPSRet) : U@CPSRet

    protected object Halt extends Then{
      def Then[U >: T](func: => U@CPSRet) = createHalt(func)
    }

    sealed trait Then{
      def Then[U >: T]( func : => U@CPSRet) : Else[U]@CPSRet
    }
  }

  private class ElseC[+T] extends Else[T]{ self =>
    protected def createHalt[U >: T](prevVal : U) = new Else[U]{
      protected def createHalt[V >: U](func: V) = this
      def ElseIf(eval : => scala.Boolean@CPSRet) = Halt
      def Else[V >: U](func : => V@CPSRet) = prevVal
    }

    private object Continue extends Then {
      def Then[U >: T]( func : => U@CPSRet ) = self
    }

    def ElseIf(condition : => scala.Boolean@CPSRet) = if (condition) Halt else Continue
    def Else[U >: T](func : => U@CPSRet) = func
  }

  def Then[T]( func : => T@CPSRet) : Else[T]@CPSRet =
    new ElseC[T] ElseIf condition Then func
}
