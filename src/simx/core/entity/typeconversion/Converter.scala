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

import reflect.runtime.universe.TypeTag

/**
 * @author dwiebusch
 * date: 27.08.2010
 *
 * NOTE: Generally the type parameter O represents the type used within the Simulator core (O for Ontology). On the
 * other hand T stands for the component specific type.
 *
 */

/**
 *  base for converters and reverters. as stated above, tManifest is the manifest representing the components
 * type, whereas oManifest represents the type used within the Simulator core
 */
trait ConverterBase{
  protected type localType
  protected type globalType
  protected def tManifest : TypeTag[localType]
  protected def oManifest : TypeTag[globalType]
  val localRepresentations : List[ConvertibleTrait[localType]]
  val globalRepresentation : Option[ConvertibleTrait[globalType]]

  def doRepresentationsMatch[PT, PO](local : ConvertibleTrait[PT], global : ConvertibleTrait[PO]) : Boolean = {
    if (localRepresentations.exists{ lr => lr equals local})
      if (globalRepresentation.collect{ case gr => gr.getBase equals global}.getOrElse(false))
        return true
    false
  }
}

/**
 *  base class for user (e.g. component developer) defined converters. provides constructor to make creating
 * converters a lot easier. A Converter has to provide convert and revert methods
 */
abstract class Converter[T, O] private[typeconversion](override val localRepresentations : List[ConvertibleTrait[T]],
                                                       override val globalRepresentation : Option[ConvertibleTrait[O]])
                                                      (implicit val tManifest : TypeTag[T],
                                                       implicit val oManifest : TypeTag[O])
  extends IConverter[T, O] with IReverter[T, O]
{
  protected type localType = T
  protected type globalType = O

  /**
   * constructor for converters
   *
   * @param c1 the ConvertibleTrait representing the type used within the component
   * @param c2 the ConvertibleTrait representing the type used within the Simulator core
   */
  def this(c1 : ConvertibleTrait[T], c1s : ConvertibleTrait[T]*)(c2 : ConvertibleTrait[O]) =
    this(c1 :: List(c1s :_*), Some(c2))(c1.typeTag, c2.typeTag)

//  def this(c1 : ConvertibleTrait[T], c1s : ConvertibleTrait[T]*)(implicit c2 : ClassManifest[O]) =
//    this(c1 :: List(c1s :_*), None)(c1.typeinfo, c2)
}

//object ConverterRegistry{
//  final def register[T, O](c : Converter[T, O]){
//    Converter.register(c)
//  }
//}
/**
 *  The Converter object is used for process-wide registration of converters. Every Converter is registered here
 * to avoid the necessity of specifiying the converter to be used (unless one wants to do so)
 */
object Converter{
  private type IDType = Symbol
  //! the map of registered converters
  private var registeredConverters = Map[IDType, Map[IDType, ConverterBase]]()

  private def getId(c : ConvertibleTrait[_]) : IDType = Symbol(c.getClass.getPackage.getName + "." + c)

  /**
   *  register the provided converter
   * @param c the converter to be registered
   *
   */
  private[typeconversion] def register( c : ConverterBase) {
    synchronized {
      registeredConverters = c.localRepresentations.foldLeft(registeredConverters){
        (map, elem) => map.updated(getId(elem),
          map.getOrElse(getId(elem), Map[IDType, ConverterBase]()).updated(getId(c.globalRepresentation.get.getBase), c)
        )
      }
    }
  }


  /**
   * returns a matching converter
   * @param inputHint a ConvertibleTrait describing the input type
   * @param outputHint a ConvertibleTrait describing the output type
   * @return a converter which is capable of converting the input type into the output type
   */
  def apply[T, O](inputHint : ConvertibleTrait[T], outputHint : ConvertibleTrait[O]) : IConverter[T, O] =
    if (NC._canConvert(inputHint, outputHint))
      NC.asInstanceOf[IConverter[T, O]]
    else
      registeredConverters.
        getOrElse(getId(inputHint), throw NoConverterFoundException(inputHint, outputHint)).
        getOrElse(getId(outputHint.getBase), throw NoConverterFoundException(inputHint, outputHint)).asInstanceOf[IConverter[T, O]]
}

/**
 *  The Reverter object is used for process-wide registration of reverters. Every Reverter is registered here
 * to avoid the necessity of specifiying the reverter to be used (unless one wants to do so)
 */
object Reverter{
  private type IDType = Symbol
  //! the list of registered reverters
  private var registeredReverters = Map[IDType,  Map[IDType, ConverterBase]]()

  private def getId(c : ConvertibleTrait[_]) : IDType =
    Symbol(c.getClass.getPackage.getName + "." + c)

  /**
   *  register the provided reverter
   * @param c the reverter to be registered
   */
  private[typeconversion] def register( c : ConverterBase) {
    synchronized{
      registeredReverters = c.localRepresentations.foldLeft(registeredReverters){
        (map, elem) => map.updated(getId(elem),
          map.getOrElse(getId(elem), Map[IDType, ConverterBase]()).updated(getId(c.globalRepresentation.get.getBase), c)
        )
      }
    }
  }

  /**
   * returns a matching reverter
   * @param outputHint a ConvertibleTrait describing the output type
   * @param inputHint a ConvertibleTrait describing the input type     *
   * @return a converter which is capable of converting the input type into the output type
   */
  def apply[T, O](outputHint : ConvertibleTrait[T], inputHint : ConvertibleTrait[O]) : IReverter[T, O] =
    if (NC._canConvert(outputHint, inputHint))
      NC.asInstanceOf[IReverter[T, O]]
    else registeredReverters.getOrElse(getId(outputHint), throw NoReverterFoundException(inputHint, outputHint)).
      getOrElse(getId(inputHint.getBase), throw NoReverterFoundException(inputHint, outputHint)).asInstanceOf[IReverter[T, O]]
}

/**
 *  the internal base class of converters
 */
protected[entity] trait IConverter[-T, +O] extends ConverterBase{
  /**
   * this method may be overwritten to add additional checking when a converter is tested to match the conversion task
   * @param from a description of the input type
   * @param to a description of the output type
   * @return true if the converter can convert from from to to ;-) false if it can't
   */
  def canConvert(from : ConvertibleTrait[_], to : ConvertibleTrait[_]) : Boolean  =
    doRepresentationsMatch(from, to)

  /**
   * the actual conversion function
   * @param i the input data to be converted
   * @return the converted data
   */
  def convert( i : T ) : O

  /**
   * internal check for convertibility, based on type information
   *
   * calls the canConvert method
   *
   * @param from information on the input type
   * @param to information on the output type
   * @return true if the converter can convert from from to to ;-) false if it can't
   */
  private[typeconversion] def _canConvert(from : ConvertibleTrait[_], to : ConvertibleTrait[_]) : Boolean =
    if (from.typeTag.tpe <:< tManifest.tpe && oManifest.tpe <:< to.typeTag.tpe) canConvert(from, to) else false

  //! register this converter
  Converter.register(this)
}

/**
 *  the internal base class of reverters
 */
protected[entity] trait IReverter[+T, -O] extends ConverterBase{
  /**
   * this method may be overwritten to add additional checking when a reverter is tested to match the reversion task
   *
   * @param to a description of the output type
   * @param from a description of the input type
   * @return true if the reverter can revert from from to to ;-) false if it can't
   */
  def canRevert(to : ConvertibleTrait[_], from : ConvertibleTrait[_]) : Boolean =
    doRepresentationsMatch(to, from)

  /**
   * the actual reversion function
   * @param i the input data to be reverted
   * @return the reverted data
   */
  def revert( i : O ) : T

  /**
   * internal check for revertibility, based on type information
   *
   * calls the canRevert method
   *
   * @param to information on the output type
   * @param from information on the input type
   * @return true if the reverter can revert from from to to ;-) false if it can't
   */
  private[typeconversion] def _canRevert(to : ConvertibleTrait[_], from : ConvertibleTrait[_]) : Boolean =
    if (from.typeTag.tpe <:< oManifest.tpe && tManifest.tpe <:< to.typeTag.tpe ) canRevert(to, from) else false

  //! register this reverter
  Reverter.register(this)
}


/**
 * the null converter (does not con- or revert anything but provides input- as output-data)
 */
private object NC extends Converter[Any, Any](Nil, None){
  override private[typeconversion] def _canConvert(from: ConvertibleTrait[_], to: ConvertibleTrait[_]) = canConvert(from, to)
  override private[typeconversion] def _canRevert(to: ConvertibleTrait[_], from: ConvertibleTrait[_])  = canRevert(from, to)
  override def canConvert(from: ConvertibleTrait[_], to: ConvertibleTrait[_]) : Boolean = from.typeTag.tpe <:< to.typeTag.tpe
  override def canRevert(to: ConvertibleTrait[_], from: ConvertibleTrait[_])  = from.typeTag.tpe <:< to.typeTag.tpe
  def convert(i: Any) = i
  def revert(i: Any)  = i
}

// Some Exception definitions
case class NoConverterFoundException[T](from : TypeInfo[T, _ <: T], to : TypeInfo[T, _ <: T]) extends Exception(
  from + "(" + from.typeTag +") => "  + to + "(" + to.typeTag +")"
)
case class NoReverterFoundException[T](from : TypeInfo[T, _ <: T], to : TypeInfo[T, _ <: T]) extends Exception(
  from + "(" + from.typeTag +") => "  + to + "(" + to.typeTag +")"
)