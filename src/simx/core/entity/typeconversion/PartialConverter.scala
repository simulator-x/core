///*
// * Copyright 2013 The SIRIS Project
// *
// *    Licensed under the Apache License, Version 2.0 (the "License");
// *    you may not use this file except in compliance with the License.
// *    You may obtain a copy of the License at
// *
// *        http://www.apache.org/licenses/LICENSE-2.0
// *
// *    Unless required by applicable law or agreed to in writing, software
// *    distributed under the License is distributed on an "AS IS" BASIS,
// *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// *    See the License for the specific language governing permissions and
// *    limitations under the License.
// *
// * The SIRIS Project is a cooperation between Beuth University, Berlin and the
// * HCI Group at the University of Würzburg. The project is funded by the German
// * Federal Ministry of Education and Research (grant no. 17N4409).
// */
//
package simx.core.entity.typeconversion

import simx.core.svaractor.{SVar, SVarActor}
import simx.core.entity.Entity
import scala.reflect.ClassTag

/**
 * User: dwiebusch
 * Date: 06.09.13
 * Time: 12:54
 */

abstract class PartialConverter[T : ClassTag, O : ClassTag] {
  self =>
  protected var valueBuffer : Option[T] = None

  protected[typeconversion] def update(newVal : O){
    valueBuffer = Some(revert(newVal))
  }

  /**
   * the actual conversion function
   * @param i the input data to be converted
   * @return the converted data
   */
  def convert(i: T): O

  final def insert(i : T) : O = {
    convert(i)
  }

  /**
   * the actual reversion function
   * @param i the input data to be reverted
   * @return the reverted data
   */
  def revert(i: O): T

  def apply(base : PartialConverterBase[O], c : ConvertibleTrait[T]) : PartialConverter[T, O] = {
    new PartialConverter[T, O] {
      def convert(i: T): O = {
        val retVal = self.insert(i)
        base.feedback(retVal, this)
        retVal
      }

      /**
       * the actual reversion function
       * @param i the input data to be reverted
       * @return the reverted data
       */
      def revert(i: O): T = self.revert(i)
    }
  }
}

protected class PartialConverterBase[L](svar : SVar[L], actorContext : SVarActor){
  private var registeredConverters = Set[PartialConverter[_, L]]()
  private var valueBuffer : Option[L] = None
  svar.get{ newValue => valueBuffer = Some(newValue) }(actorContext)

  svar.observe{
    newVal =>
      valueBuffer = Some(newVal)
      registeredConverters.foreach(_.update(newVal))
  }(actorContext)

  protected[typeconversion] def feedback(value : L, sender : PartialConverter[_, _]){
    svar.set(value, false)(actorContext)
    registeredConverters.foreach{
      converter =>
        if (converter != sender)
          converter.update(value)
    }
  }

  protected[typeconversion] def addConverter[T](converter : PartialConverter[T, L]){
    registeredConverters = registeredConverters + converter
  }

  def removeConverter[T](converter : PartialConverter[T, L]){
    registeredConverters = registeredConverters - converter
  }
}

trait PartialConverterSupport extends SVarActor{
  protected var svar2converterbase    = Map[SVar[_], PartialConverterBase[_]]()
  protected var converterBases        = Map[ConvertibleTrait[_], PartialConverterBase[_]]()
  protected var registeredConverters  = Map[ConvertibleTrait[_], Map[ConvertibleTrait[_], PartialConverter[_,_]]]()

  def registerPartialConverter[L, T](from : ConvertibleTrait[T], to : ConvertibleTrait[L], c : PartialConverter[T, L]){
    registeredConverters = registeredConverters.updated(from, registeredConverters.getOrElse(from, Map()).updated(to, c))
  }
//
//  def extract[L](c : ConvertibleTrait[L]) : A[L] =
//    A[L](c)
//
//  case class A[L] protected[PartialConverterSupport](c : ConvertibleTrait[L]){
//    def from(e : Entity) = {
//      val svar = e.get(c).head
//      val cbase = svar2converterbase.get(svar) match {
//        case Some(x : PartialConverterBase[L]) => x
//        case None =>
//          val retVal = new PartialConverterBase[L](svar, actorContext)
//          svar2converterbase = svar2converterbase.updated(svar, retVal)
//          retVal
//        case x => throw new Exception("Unexpected type " + x.getClass.getCanonicalName)
//      }
//      new B[L](svar, cbase, c)
//    }
//  }
//
//  class B[L] protected[PartialConverterSupport](svar : SVar[L], cbase : PartialConverterBase[L], oc : ConvertibleTrait[L]){
//    def as[T]( c : ConvertibleTrait[T]) : SVar[T] = {
//      registeredConverters.get(oc) match {
//        case None => throw new NoSuchElementException()
//        case Some(map) => map.get(c) match {
//          case Some(convTemplate : PartialConverter[T, L]) =>
//            val converter = convTemplate.apply(cbase, c)
//            cbase.addConverter[T](converter)
//            new PartiallyConvertedSVar[L, T](svar, converter.insert, converter.revert ) {
//              def containedValueManifest = c.typeinfo
//            }
//          case _ => throw new NoSuchElementException
//        }
//      }
//    }
//  }
}



