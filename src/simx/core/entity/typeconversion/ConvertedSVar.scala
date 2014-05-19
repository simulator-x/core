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
class ConvertedSVar[O, T](val wrappedSVar : SVar[O], c : ConversionInfo[T, O])
  extends PartiallyConvertedSVar[O, T](wrappedSVar, c.accessConverter().convert, c.accessReverter().revert)
{
  //! constructor for ease of use
  protected[entity] def this( wrappedSVar : SVar[O], par : Require[O, T] ) = this(wrappedSVar, par.wrapped)
  //! conversions, implicit for nicer code
  val containedValueManifest: ClassTag[T]  = c.from.classTag

  override def as[T2](cInfo: ConversionInfo[T2, T]) : SVar[T2] =
    wrappedSVar as c.to.requiredAs(cInfo.from).wrapped
}

protected[core] abstract class PartiallyConvertedSVar[O, T](wrappedSVar : SVar[O],
                                                            convert : (T) => O,
                                                            revert  : (O) => T)
  extends SVar[T]
{
  //! reversions, implicit for nicer code
  private def converter( input : T ) : O = convert(input)
  //! loop method calls through to the wrapped svar
  //def update( updateMethod : T => T )  { wrappedSVar.update( (v : O) => updateMethod(revert(v))) }
  //! loop method calls through to the wrapped svar
  def observe(handler: (T) => Unit, ignoredWriters : Set[SVarActor.Ref])(implicit actorContext : SVarActor) =
    wrappedSVar.observe( v => handler(revert(v)), ignoredWriters)(actorContext)
  //! loop method calls through to the wrapped svar
  def get(consume : (T) => Unit)(implicit actorContext : SVarActor){
    wrappedSVar.get( v => consume(revert(v)))(actorContext)
  }
  //! loop method calls through to the wrapped svar
  protected[core] def set(value : T, forceUpdate : Boolean = false)(implicit actorContext : SVarActor) =
    wrappedSVar.set(converter(value), forceUpdate)(actorContext)
  //! loop method calls through to the wrapped svar
  def ignore()(implicit actorContext : SVarActor){
    wrappedSVar.ignore()(actorContext)
  }
  //! redicrect id lookups to wrapped svar
  final def id = wrappedSVar.id

  val initialOwner = wrappedSVar.initialOwner
}