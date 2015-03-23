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

package simx.core.svaractor.instantaccess

import java.util.UUID

import simx.core.entity.typeconversion.ConvertedSVar
import simx.core.entity.description.SVal._
import simx.core.svaractor._
import TimedRingBuffer._
import simx.core.svaractor.handlersupport.Types.CPSRet

import scala.reflect._
import scala.util.continuations._

/**
 *
 * Created by dennis on 07.10.14.
 */
trait InstantAccess extends SVarActor{
  private val cachedValues = collection.mutable.Map[UUID, Option[ContentType[_]]]()

  object try_protector {
    def apply[A,B](comp: => A @cpsParam[B, B]):A @cpsParam[B, B] = {
      comp
    }
  }

  override protected def addHandler[T: ClassTag](handler: Function[T, Any@CPSRet]) =
  {
    handler match{
      case pf : PartialFunction[_, _] => updateHandlers( classTag[T].runtimeClass, (generateId(), {
        (msg: Any) => try {
          reset[Any, Any] {
            val ret = handler(msg.asInstanceOf[T])
            shift { (x: Unit => Any) => ret }
          }
        } catch {
          case SVarValueNotCachedException(sVar) =>
//            println("handling exception")
            myObserve(sVar, msg)
        }
      }))
      case f => updateHandlers( classTag[T].runtimeClass, (generateId(), {
        (msg : Any) => try {
          reset[Any, Any]{
            val ret = handler(msg.asInstanceOf[T])
            shift{(x : Unit => Any) => ret }
          }
        } catch {
          case SVarValueNotCachedException(sVar) =>
//            println("handling exception")
            myObserve(sVar, msg)
        }
      }))
    }
  }

  //    super.addHandler[T]{ msg : T =>
  //      try_protector[Any, Any]{
  //        try {
  //          handler(msg)
  //        } catch {
  //          case SVarValueNotCachedException(sVar) =>
  //            println("handling exception")
  //            myObserve(sVar, msg)
  //        }
  //      }
  //    }


  override private[svaractor] def toRemovableTuple(pf: handlerC_t, manifest: Class[_], id: IdType) = id -> new PartialFunction[Any, Any] {
    override def isDefinedAt(x: Any) = pf.isDefinedAt(x)
    override def apply(msg: Any) = try {
      reset[Any, Any] {
        val ret = pf(msg)
        removeHandler(id, manifest)
        shift { (x: Unit => Any) => ret }
      }
    } catch {
      case SVarValueNotCachedException(sVar) =>
//        println("handling exception")
        addSingleUseHandler(pf, manifest)
        myObserve(sVar, msg)
    } 
  }

//  override protected[svaractor] def addSingleUseHandler[T](pf: handlerC_t, manifest : Class[_]) {
//      super.addSingleUseHandler[T]({
//        case msg if pf.isDefinedAt(msg) =>
//          try {
//            pf(msg)
//          } catch {
//            case SVarValueNotCachedException(sVar) =>
//              println("handling exception")
//              addSingleUseHandler(pf, manifest)
//              myObserve(sVar, msg)
//          }
//      } : handlerC_t, manifest)
//    }

  protected def access[T : ClassTag](stateParticle : StateParticle[T]) : T = stateParticle match  {
    case converted : ConvertedSVar[_, T] =>
      var value : Option[T] = None
      get(converted)(v => value = Some(v))
      value.get
    case sval: SValType[T] =>
      sval.typedValue
    case svar : SVar[T] =>
      cachedValues.getOrElse(svar.id, throw new SVarValueNotCachedException(svar) ).get._1.asInstanceOf[T]
  }

  private def myObserve[T : ClassTag](sVar : SVar[T], msg : Any): Unit ={
    if (cachedValues.contains(sVar.id))
      self ! msg // this should be impossible...
    else {
      cachedValues.update(sVar.id, None)
      sVarObserveHandlers.get(sVar) match {
        case Some(previousHandler: (ContentType[T] => Unit)) =>
          addSVarObserveHandler(sVar) { updatedValue =>
            addSVarObserveHandler(sVar)(updateCacheAndThen(sVar, _, previousHandler))
          }
          get(sVar){ v =>
            updateCacheAndThen(sVar, v -> Now)
            self ! msg
          }
        case None =>
          sVar.observe{ (v, t) =>
            addSVarObserveHandler(sVar)(updateCacheAndThen(sVar, _))
            updateCacheAndThen(sVar, v -> t)
            self ! msg
          }
      }
    }
  }

  private def updateCacheAndThen[T](sVar : SVar[T], value : ContentType[T],
                                    andThen : ContentType[T] => Unit = (_ : ContentType[T])  => {}){
    cachedValues.update(sVar.id, Some(value))
    andThen(value)
  }

}

private case class SVarValueNotCachedException[T](svar : SVar[T])
  extends Exception("SVar " + svar + " was not cached, yet. You should not see this, though.")
