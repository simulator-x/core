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
//package simx.core.entity.typeconversion
//
//import scala.reflect.runtime.universe.TypeTag
//
///**
// * User: dwiebusch
// * Date: 06.09.13
// * Time: 12:54
// */
//abstract class ChainableConverter[T : TypeTag, O : TypeTag](override val localRepresentations : List[ConvertibleTrait[T]],
//                                                            override val globalRepresentation : Option[ConvertibleTrait[O]])
//                                                           //( getPrevValue : => Option[O], setPrevValue : O => ())
//  extends Converter[T, O](localRepresentations, globalRepresentation) with Chainable
//{
//  def partialConvert(i : T, prevValue : Option[O]) : O
//
// // def getPVal: Option[O] = getPrevValue
//
//  def setPVal(newVal: O) {
//    //setPrevValue(newVal)
//  }
//}
//
//trait Chainable[O]{
//  self =>
//  val globalRepresentation : ConvertibleTrait[O]
//
//  def getPVal : Option[O]
//  def setPVal(newVal : O)
//
//  def chain[T : TypeTag](next : ChainableConverter[T, O]) : ChainableConverter[T, O] = {
//    new ChainableConverter[T, O](next.localRepresentations, Some(globalRepresentation)){
//      /**
//       * the actual conversion function
//       * @param i the input data to be converted
//       * @return the converted data
//       */
//      def convert(i: T): O = {
//        val retVal = next.partialConvert(i, getPVal)
//        setPVal(retVal)
//        retVal
//      }
//
//      /**
//       * the actual reversion function
//       * @param i the input data to be reverted
//       * @return the reverted data
//       */
//      def revert(i: O): T = {
//        setPVal(i)
//        next.revert(i)
//      }
//
//      override def getPVal: Option[O] = self.getPVal
//
//      override def setPVal(newVal: O) { self.setPVal(newVal) }
//
//      def partialConvert(i: T, prevValue: Option[O]): O = next.partialConvert(i, prevValue)
//    }
//  }
//
//}
//
//abstract class ConverterChain[O](val globalRepresentation : ConvertibleTrait[O]) extends Chainable[O]{
//  private var pVal : Option[O] = None
//
//}
