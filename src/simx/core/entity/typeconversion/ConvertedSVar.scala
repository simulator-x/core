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

package simx.core.entity.typeconversion

import simx.core.svaractor.{SVarActor, SVar}
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag

/* author: dwiebusch
 * date: 27.08.2010
 */

/**
 *  class that wraps a svar with an converter
 *
 * Note:
 * internalType = O
 * externalType = T
 */
class ConvertedSVar[O, T]( val wrappedSVar : SVar[O], c : ConversionInfo[T, O]) extends SVar[T]{
  /**
   * constructor for ease of use
   */
  protected[entity] def this( wrappedSVar : SVar[O], par : Require[O, T] ) = this(wrappedSVar, par.wrapped)
  //! the convert function
  val convert : (T) => O = c.accessConverter().convert
  //! the revert function
  val revert  : (O) => T = c.accessReverter().revert
  //! conversions, implicit for nicer code
  private implicit def converter( input : (T) => Unit ) = (x : O) => input( revert(x) )
  //! reversions, implicit for nicer code
  private implicit def converter( input : T ) : O = convert(input)
  //! loop method calls through to the wrapped svar
  //def update( updateMethod : T => T )  { wrappedSVar.update( (v : O) => updateMethod(revert(v))) }
  //! loop method calls through to the wrapped svar
  def observe(handler: (T) => Any)(implicit actorContext: SVarActor) {
    observe(handler, Set())
  }

  def observe(handler: (T) => Any, ignoredWriters : Set[SVarActor.Ref] = Set())(implicit actorContext : SVarActor){
    wrappedSVar.observe( v => handler(revert(v)), ignoredWriters)(actorContext)
  }
  //! loop method calls through to the wrapped svar
  //def owner(owner: SVarActor)          { wrappedSVar.owner(owner) }
  //! loop method calls through to the wrapped svar
  def get(consume : (T) => Any)(implicit actorContext : SVarActor){
    wrappedSVar.get( v => consume(revert(v)))(actorContext)
  }
  //! loop method calls through to the wrapped svar
  def set(value : T)(implicit actorContext : SVarActor){
    wrappedSVar.set(value)(actorContext)
  }
  //! loop method calls through to the wrapped svar
  def ignore()(implicit actorContext : SVarActor){
    wrappedSVar.ignore()(actorContext)
  }
  //! loop method calls through to the wrapped svar
  //def owner()                          = wrappedSVar.owner()
  //! set the classmanifest of the contained value
  val containedValueManifest: ClassTag[T]  = c.from.typeinfo
  //! loop method calls through to the wrapped svar
  //def observe(ignoredWriters : Set[Actor])(handler: (T) => Unit)  { wrappedSVar.observe(ignoredWriters)(handler) }
  //! redicrect id lookups to wrapped svar
  final def id = wrappedSVar.id

  val initialOwner = wrappedSVar.initialOwner
}