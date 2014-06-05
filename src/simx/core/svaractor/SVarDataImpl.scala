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

package simx.core.svaractor
import scala.ref.WeakReference
import simx.core.entity.description.{SValBase, SVal}
import simx.core.entity.typeconversion.ConvertibleTrait

/**
 * @author Dennis Wiebusch
 * @author Stephan Rehfeld
 * date: 03.08.2010
 */


protected class SVarDataImpl[T](private var data : T, val svar : WeakReference[SVar[T]],
                                typeInfo : ConvertibleTrait[T] )  extends SVarData {
  protected var observers = Map[SVarActor.Ref, Set[SVarActor.Ref]]()

  def this(sval : SVal[T], svar : WeakReference[SVar[T]]) =
    this(sval.value, svar, sval.typedSemantics.asConvertibleTrait)

  def write[V]( writer: SVarActor.Ref, value : V )(implicit actor : SVarActor) {
    data = value.asInstanceOf[T]
    notifyWrite(writer)
  }

  def read[V] : V =
    data.asInstanceOf[V]

  def readFull[V] : SVal[V] =
    typeInfo.asInstanceOf[ConvertibleTrait[V]](read)

  def notifyWrite(writer: SVarActor.Ref)(implicit actor : SVarActor) {
    svar.get match {
      case Some(ref) => observers.foreach{ kvPair =>
        if(!kvPair._2.contains(writer))
          actor.notifyObserver(kvPair._1, NotifyWriteSVarMessage[T]( ref, data )(actor.self) ) }
      case None =>
    }
  }

  def removeObserver(a: SVarActor.Ref) {
    observers = observers - a
  }

  def addObserver(a: SVarActor.Ref, ignoredWriters: Set[SVarActor.Ref]) {
    observers = observers + (a -> ignoredWriters)
  }

  def isObservedBy( a : SVarActor.Ref ) : Boolean =
    observers.contains(a)

  def getObservers =
    observers
}
