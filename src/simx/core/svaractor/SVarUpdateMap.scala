/*
 * Copyright 2013 The SIRIS Project
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

import scala.collection.mutable

import simx.core.entity.Entity
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling


/**
 * User: dwiebusch
 * Date: 13.05.13
 * Time: 10:49
 */
trait SVarUpdateMap extends SVarActor with EntityUpdateHandling {
  private type updateHandlerType[T] = T => Any
  private val storedValues = mutable.WeakHashMap[SVar[_], Any]()
  private val updateFuncs = mutable.WeakHashMap[SVar[_], updateHandlerType[_]]()

  private def execOnFirstSVar[T, U](e : Entity, svarDesc : ConvertibleTrait[T])(handler : SVar[T] => U) {
    e.getSVars(svarDesc).headOption.collect {
      case (_, svar: SVar[T]) => handler(svar)
    }
  }

  def execHandlerFor[T](sVar : SVar[T]){
    updateFuncs.get(sVar).collect{ case handler : (T => Any)@unchecked => getValue(sVar).collect{ case value => handler(value) } }
  }

  def setHandlerFor[T](svar : SVar[T])(handler : T => Any){
    updateFuncs.update(svar, handler)
  }

  def addSVar[T](svar : SVar[T], execOnFirstGet : Boolean = false, execOnUpdate : Boolean = false)(handler : T => Any){
    updateFuncs.update(svar, handler)
    svar.get{     value => storedValues.update(svar, value); if(execOnFirstGet) handler(value) }
    svar.observe{ value => storedValues.update(svar, value); if( execOnUpdate ) handler(value) }
  }

  def removeSVar[T](sVar : SVar[T]){
    if (storedValues.remove(sVar).isDefined && updateFuncs.remove(sVar).isDefined)
      removeObserveHandler(sVar)
  }

  def removeObserveHandler[T](svar : SVar[T]){
    svar.ignore()
  }

  def getValue[T](svar : SVar[T]) : Option[T] = storedValues.get(svar) match {
    case Some(value) => Some(value.asInstanceOf[T])
    case _ => None
  }

  //Convinience methods
  def addSVar[T](e : Entity, svarDesc : ConvertibleTrait[T])(handler : T => Any){
    execOnFirstSVar(e, svarDesc)(addSVar(_)(handler))
  }

  def addSVar[T](e : Entity, svarDesc : ConvertibleTrait[T], execOnFirstGet : Boolean)(handler : T => Any){
    execOnFirstSVar(e, svarDesc)(addSVar(_, execOnFirstGet)(handler))
  }

  def addSVar[T](e : Entity, svarDesc : ConvertibleTrait[T], execOnFirstGet : Boolean, execOnUpdate : Boolean)(handler : T => Any){
    execOnFirstSVar(e, svarDesc)(addSVar(_, execOnFirstGet, execOnUpdate)(handler))
  }

  def removeSVar[T](e : Entity, svarDesc : ConvertibleTrait[T]){
    execOnFirstSVar(e, svarDesc)(removeSVar[T])
  }

  def removeObserveHandler[T](e : Entity, svarDesc : ConvertibleTrait[T]){
    execOnFirstSVar(e, svarDesc)(removeObserveHandler[T])
  }

//  def getValue[T](e : Entity, svarDesc : ConvertibleTrait[T]) : Option[T] =
//    execOnFirstSVar(e, svarDesc)(getValue[T]).getOrElse(None)

}
