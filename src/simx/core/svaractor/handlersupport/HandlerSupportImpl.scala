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

import scala.collection.mutable
import java.util.UUID
import scala.reflect.{classTag, ClassTag}

/* author: dwiebusch
 * date: 19.09.2010
 */

trait HandlerSupportImpl extends HandlerSupport{
  private type IdHandlerPair   = (IdType, handlerType[Any])
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

  protected def addHandler[T : ClassTag](handler: Function[T, Any]){
    handler match{
      case pf : PartialFunction[_, _] => updateHandlers( classTag[T].runtimeClass, (generateId(), handler.asInstanceOf[PartialFunction[Any, Any]] ) )
      case f => updateHandlers( classTag[T].runtimeClass, (generateId(), (msg : Any) => handler(msg.asInstanceOf[T]) ) )
    }
  }

  protected def addHandlerPF[T : ClassTag]( pf: PartialFunction[T, Any] ){
    addHandler[T](pf)
  }

  protected def removeHandler( id: Long, manifest : Class[_]) {
    handlers.update(manifest, handlers.getOrElse(manifest, Nil).filterNot( _._1 == id ) )
  }

  protected def addSingleUseHandler[T : ClassTag]( f: handlerType[T] ) {
    addSingleUseHandler(wrap(f), classTag[T].runtimeClass)
  }

  protected def addSingleUseHandlerPF[T : ClassTag]( pf: PartialFunction[T, Any] ) {
    addSingleUseHandler(pf.asInstanceOf[PartialFunction[Any, Any]], classTag[T].runtimeClass)
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
    }
  }

  //private def storeContinuation( hOpt : HandlerOption ) {
  //  hOpt collect { case (handler, manifest) => addSingleUseHandler(handler, manifest) }
  //}

  private def addSingleUseHandler[T](pf: handlerC_t, manifest : Class[_]) {
    updateHandlers(manifest, toRemovableTuple(pf, manifest, generateId() ), append = true )
  }

  private def toRemovableTuple( pf : handlerC_t, manifest : Class[_], id : IdType ) =
    id -> pf.andThen{ _ => removeHandler(id, manifest) }

  private def generateId() : IdType =
    (lastId +=1, lastId)._2

  private def updateHandlers( manifest : Class[_], toAdd : IdHandlerPair, append : Boolean = false ) {
    handlers.update(manifest, if (append) handlers.getOrElse(manifest, Nil) :+ toAdd else toAdd :: handlers.getOrElse(manifest, Nil) )
  }

//  private[svaractor] def continueWith[A, T]( f : Function[A, T])(implicit manifest : ClassTag[A]) : T @HandlerContinuation =
//    shift { (fun : T => HandlerOption) => Some( wrap(f) andThen fun andThen storeContinuation, manifest ) }

  private type tmpType = PartialFunction[Any, _]

  private[handlersupport] def applyHandlerList( list : handlerList, msg : Any ) : Any = list match {
    case Nil => ()
    case (_, handler) :: tail => handler match {
      case pf : tmpType => if( pf isDefinedAt msg ) pf( msg ) else applyHandlerList(tail, msg)
      case f => f(msg)
    }
  }

  private def wrap[T, U](f : Function[T, U])(implicit manifest : ClassTag[T]) = new PartialFunction[Any, U]{
    def isDefinedAt( x : Any ) =
      if (manifest.runtimeClass isAssignableFrom x.getClass) definedAt( x.asInstanceOf[T] ) else false
    def apply(v1: Any) = f.apply(v1.asInstanceOf[T])
    private val definedAt : T => Boolean = f match {
      case pf : PartialFunction[T, U] => pf.isDefinedAt
      case _ => ( x : Any ) => true
    }
  }
}
