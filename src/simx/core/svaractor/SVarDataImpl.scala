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

import simx.core.entity.description.SVal.SValType
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.svaractor.TimedRingBuffer._

import scala.ref.WeakReference


protected class SVarDataImpl[T](private val data : TimedRingBuffer[T], val svar : WeakReference[SVar[T]],
                                typeInfo : ConvertibleTrait[T] )  extends SVarData {
  protected var observers = Map[SVarActor.Ref, Set[SVarActor.Ref]]()

  def this(data : T, timeStamp : Time, svar : WeakReference[SVar[T]], typeInfo : ConvertibleTrait[T],
           bufferLength : BufferMode = TimedRingBuffer.Unbuffered ) =
    this(TimedRingBuffer(data, timeStamp, bufferLength), svar, typeInfo)

  def this(sval : SValType[T], timeStamp : Time, svar : WeakReference[SVar[T]]) =
    this(sval.value, timeStamp, svar, sval.typedSemantics.asConvertibleTrait)

  def this(sval : SValType[T], timeStamp : Time, svar : WeakReference[SVar[T]], bufferLength : BufferMode) =
    this(sval.value, timeStamp, svar, sval.typedSemantics.asConvertibleTrait, bufferLength)

  def getBufferSetting : BufferMode =
    if (data.getMaxTime > 0 ) MaxTime(data.getMaxTime) else Unbuffered

  def write[V]( writer: SVarActor.Ref, value : V, at : Time )(implicit actor : SVarActor) {
    data.put(value.asInstanceOf[T], at)
    notifyWrite(writer)
  }

  def read[V](at : Time, accessMethod : AccessMethod) : ContentType[V] =
    data(at, accessMethod).asInstanceOf[ContentType[V]]

  def readFull[V](at: Time, accessMethod : AccessMethod) : ContentType[SValType[V]] = {
    val value = read[V](at, accessMethod)
    typeInfo.asInstanceOf[ConvertibleTrait[V]](value._1) -> value._2
  }

  protected def notifyWrite(writer: SVarActor.Ref)(implicit actor : SVarActor) {
    svar.get match {
      case Some(ref) => observers.foreach{ kvPair =>
        if(!kvPair._2.contains(writer))
          actor.notifyObserver(kvPair._1, NotifyWriteSVarMessage[T]( ref, data.getHead )(actor.self) ) }
      case None =>
    }
  }

  def removeObserver(a: SVarActor.Ref) {
    observers = observers - a
  }

  def addObserver(a: SVarActor.Ref, ignoredWriters: Set[SVarActor.Ref])(implicit actor : SVarActor) {
    svar.get.collect { case ref => actor.notifyObserver(a, NotifyWriteSVarMessage(ref, data.getHead)(actor.self)) }
    observers = observers + (a -> ignoredWriters)
  }

  def isObservedBy( a : SVarActor.Ref ) : Boolean =
    observers.contains(a)

  def getObservers =
    observers

  override def toString: String =
    "SVarDataImpl for " + typeInfo + " with value " + data + " observed by " + observers.keys
}
