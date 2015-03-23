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

import scala.util.continuations.{cpsParam, cps}
import scala.reflect.ClassTag

object Types{
  private type hBase[T]    = PartialFunction[Any, T]
  type CPSRet              = cpsParam[Any, Any]
  type handler_t           = hBase[Unit]
  type handlerC_t          = hBase[Any@CPSRet]
//  type cpsHandler_t        = hBase[Any @HandlerContinuation]
  type HandlerOption       = Option[(handlerC_t, Class[_])]
  type HandlerContinuation = cps[HandlerOption]

}

/* author: dwiebusch
 * date: 19.09.2010
 */

trait HandlerSupport{
  type handler_t           = Types.handler_t
  type handlerC_t          = Types.handlerC_t
//  type cpsHandler_t        = Types.cpsHandler_t
  type handlerList_t       = List[(Handler, handler_t, Boolean)]
  type HandlerOption       = Types.HandlerOption
  type HandlerContinuation = Types.HandlerContinuation


  // referencing for handlers
  /**
   * This trait must be extended by data structes that identifies a registered
   * handler. The identification is used to remove a handler.
   *
   * So the operator ==, equals and hashCode must be implemented in the correct
   * way.
   */
  trait Handler

  protected type IdType          = Long
  protected type handlerType[T]  = Function[T, Any@CPSRet]
  //protected type handlerTypeC[T] = Function[T, Any @HandlerContinuation]

  // returns the reference of the registered handler
  /**
   * This method adds a handler that processes a message. The handling is
   * done by PartialFunctions that processes messages with the given data
   * type. After the function is registered a Handler is returned that
   * identifies the handler and can be used to remove it again.
   *
   * This method must not be called from outside of the actor. If it is called
   * from outside a SVarActorException is thrown.
   *
   *
   * @param handler A PartialFunction that processes messages of the given datatype
   * @return A handler to the registred handler.
   */
  protected def addHandler[T : ClassTag](handler: handlerType[T])
  protected def addHandlerPF[T  : ClassTag]( pf: PartialFunction[T, Any@CPSRet] )
  //@deprecated( "Needs to become protected in future" )
  protected def addSingleUseHandler[T : ClassTag]( f: handlerType[T] )
  //@deprecated( "Needs to become protected in future" )
  protected def addSingleUseHandlerPF[T : ClassTag]( pf: PartialFunction[T, Any@CPSRet] )

  // removes a handler using the reference given by addHandler
  /**
   * This method removes a handler from am actor. The Handler Object
   * that was created by a previous call of addHandler must be used
   * to identify the handler.
   *
   * This method must not be called from outside of the actor. If it is called
   * from outside a SVarActorException is thrown.
   *
   * @param id The id of the handler that should be removed.
   */
  protected def removeHandler(id : Long, manifest : Class[_])

  protected def applyHandlers( msg : Any ) : Any
}