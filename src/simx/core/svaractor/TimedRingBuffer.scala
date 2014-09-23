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

package simx.core.svaractor

import simx.core.svaractor.TimedRingBuffer._

/**
 * Created by dwiebusch on 11.09.14
 */
object TimedRingBuffer{
  sealed abstract class BufferMode(val getMaxBufferTime : Long, val timestoring : Boolean)
  sealed case class MaxTime(mSecs : Long) extends BufferMode(mSecs, timestoring = true)
  case object UnbufferedTimeStoring extends BufferMode(0, timestoring = true)
  case object Unbuffered extends BufferMode(0, timestoring = false)

  type ContentType[+T] = (T, Time)

  object AccessMethod extends Enumeration{
    val GetClosest = Value("GetClosest")
  }

  sealed abstract class Time {
    def reify : Time
    protected[TimedRingBuffer] def timeInMillis : Long

    def <(that : Long) : Boolean = timeInMillis < that
    def >(that : Long) : Boolean = timeInMillis > that
    def <(that : Time) : Boolean = that > this
    def >(that : Time) : Boolean = >(that.timeInMillis)
    def -(that : Time) : Long = that match {
      case UndefinedTime => timeInMillis
      case that : Time => timeInMillis - that.timeInMillis
    }
  }

  final case class At(timeInMillis : Long) extends Time{
    override def reify: Time = this
  }

  object Now extends Time {
    protected[TimedRingBuffer] def timeInMillis = System.currentTimeMillis()
    override def reify: Time = At(timeInMillis)
  }

  object UndefinedTime extends Time{
    protected[TimedRingBuffer] def timeInMillis = 0L
    override def reify: Time = this

    override def >(that: Time): Boolean = that match {
      case UndefinedTime => false
      case _ => true
    }
    override def -(that: Time): Long =
      that.timeInMillis
  }

  def apply[T](content : T, at : Time, mode : BufferMode) : TimedRingBuffer[T] =
    if (mode != Unbuffered)
      new RealTimedRingBuffer[T](Array(content -> at), mode)
    else
      new PseudoTimedRingBuffer[T](content, at, mode)

  def apply[T](content : T, initialMaxTime : BufferMode) : TimedRingBuffer[T] =
    apply(content, Now, initialMaxTime)
}

abstract class TimedRingBuffer[T]{
  type ContentType = TimedRingBuffer.ContentType[T]
  def size : Int
  def getMaxTime : Long
  def getHead : ContentType
  def setMaxTime(newValue : BufferMode) : TimedRingBuffer[T]
  def put[X <: T](value : X, timestamp : Time = Now)
  protected def _at(time : Time, accessMethod : AccessMethod = GetClosest) : ContentType

  def apply(time : Time) : ContentType =
    at(time)

  def at(time : Time) : ContentType =
    at(time, GetClosest)

  def apply(time : Time, accessMethod : AccessMethod) : ContentType =
    at(time, accessMethod)

  def at(time : Time, accessMethod : AccessMethod) : ContentType = time match {
    case Now => getHead
    case someTime : At => _at(someTime, accessMethod)
    case _ => throw new Exception()
  }
}

abstract class AccessMethod(){
  protected def inc(toInc : Int)(implicit dataSize : Int) : Int =
    if (toInc < dataSize) toInc + 1 else 0

  protected def dec(toDec : Int)(implicit dataSize : Int) : Int =
    if (toDec > 0) toDec - 1 else dataSize

  def apply[T](time : Time, data : Array[TimedRingBuffer.ContentType[T]], head : Int, tail : Int)
              (implicit size : Int) : ContentType[T]
}

object GetClosest extends AccessMethod{
  override def apply[T](time : Time, data: Array[ContentType[T]], head : Int, tail : Int)(implicit size : Int) = {
    var pos = head
    while (pos != tail && data(pos)._2 > time)
      pos = dec(pos)
    if (pos != head && time - data(pos)._2 > data(inc(pos))._2 - time)
      data(inc(pos))
    else
      data(pos)
  }
}

protected class RealTimedRingBuffer[T] (private var content : Array[TimedRingBuffer.ContentType[T]],
                                        private val mode : BufferMode) extends TimedRingBuffer[T]
{
  private var dataSize = content.length - 1
  private var maxTime = mode.getMaxBufferTime
  private var _isEmpty = false
  private var _isFull = true
  private var _size = 1
  private var head = 0
  private var tail = 0

  def setMaxTime(newValue : BufferMode) =
    if (newValue == Unbuffered || newValue == UnbufferedTimeStoring)
      TimedRingBuffer.apply(content(head)._1, content(head)._2, newValue)
    else {
      maxTime = newValue.getMaxBufferTime
      this
    }

  override def getMaxTime: Long =
    maxTime

  def getHead: ContentType =
    content(head)

  protected def _at(time : Time, accessMethod : AccessMethod = GetClosest) : ContentType =
    accessMethod.apply(time, content, head, tail)(size)

  def put[X <: T](value : X, at : Time){
    val reifiedTime = at.reify
    if (content(head)._2 > reifiedTime)
      throw new Exception("[ERROR]: Inserting old values is not supported, yet")
    updateTail(System.currentTimeMillis())
    if (isFull)
      increaseSize()
    if (!isEmpty)
      head = inc(head)
    content(head) = value -> reifiedTime
    _size += 1
    _isEmpty = false
    if (inc(head) == tail)
      _isFull = true
  }

  def isFull : Boolean =
    _isFull

  def isEmpty : Boolean =
    _isEmpty

  def size : Int =
    _size

  override def toString: String = {
    updateTail(System.currentTimeMillis())
    val dataString =
      if (isEmpty)
        " without content"
      else {
        var retVal = " and content: "
        var pos = tail
        do {
          retVal += content(pos)
          pos = inc(pos)
        } while (pos != inc(head))
        retVal
      }
    getClass.getSimpleName + " with buffer size " + size + dataString
  }

  protected def updateTail(currentTime : Long){
    val pruneTime = currentTime - maxTime
    while (tail != head && content(tail)._2 < pruneTime ) {
      tail = inc(tail)
      _isFull = false
      _size -= 1
    }
    if (content(tail)._2 < pruneTime) {
      _size = 0
      _isEmpty = true
      _isFull = false
    }
  }

  private def inc(toInc : Int) : Int =
    if (toInc < dataSize) toInc + 1 else 0

  private def dec(toDec : Int) : Int =
    if (toDec > 0) toDec - 1 else dataSize


  private def increaseSize(amount : Int = 1){
    val tmp = new Array[ContentType](content.length + amount)
    if (head > tail){
      Array.copy(content, tail, tmp, 0, head - tail + 1)
      head = head - tail
    } else if (tail > head || tail != -1) {
      val firstLength = dataSize  - tail
      Array.copy(content, tail, tmp, 0, firstLength + 1)
      Array.copy(content, 0, tmp, firstLength, head + 1)
      head = head + firstLength
    }
    dataSize += amount
    content = tmp
    tail = 0
  }
}

protected class PseudoTimedRingBuffer[T](private var content : T, private var time : Time, private val mode : BufferMode)
  extends TimedRingBuffer[T]
{
  def size: Int = 1

  def getMaxTime: Long = 0

  def put[X <: T](value: X, timestamp: Time){
    time = if (mode.timestoring) timestamp.reify else UndefinedTime
    content = value
  }

  def getHead: ContentType =
    content -> time

  protected def _at(time: Time, accessMethod: AccessMethod): ContentType =
    getHead

  def setMaxTime(newValue: BufferMode): TimedRingBuffer[T] =
    if (newValue != mode)
      TimedRingBuffer[T](content, time, newValue)
    else
      this

  override def toString: String =
    content.toString
}
