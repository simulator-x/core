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

import simx.core.entity.description.SVal
import simx.core.svaractor.TimedRingBuffer.{BufferMode, Time}
import simx.core.svaractor._
import simx.core.ontology.{Annotation, GroundedSymbol}
import simx.core.entity.Entity
import scala.reflect.ClassTag
/* author: dwiebusch
* date: 27.08.2010
*/

/**
 *  base class for conversion infos
 */
abstract class ConversionInfo[T, O]private ( private var theConverter : Option[IConverter[T, O]] = None,
                                             private var theReverter  : Option[IReverter[T, O]]  = None)
  extends Serializable
{
  protected def this(verters : Option[(IConverter[T, O], IReverter[T, O])]) =
    this(if (verters.isDefined) Some(verters.get._1) else None, if (verters.isDefined) Some(verters.get._2) else None)

  def this() =
    this(None, None)
  //! the input type
  def from : ConvertibleTrait[T]
  //! the output type
  def to   : ConvertibleTrait[O]

  /**
   * sets the con- and reverter, requiring an IConverter
   * @param c a converter which will be casted into a reverter to fill necessary variables
   * @throws NoConverterFoundException if converter did not match converting requirements
   * @throws NoReverterFoundException if converter did not match reverting requirements
   */
  protected def setVerters(c : IConverter[T, O]) = {
    val aConverter = if (c._canConvert(from, to)) c else throw new NoConverterFoundException(from, to)
    val aReverter  = c match {
      case r : IReverter[_, _] if r._canRevert(from, to) => r.asInstanceOf[IReverter[T, O]]
      case _ => throw new NoReverterFoundException(from, to)
    }
    Some(aConverter -> aReverter)
  }

  /**
   * sets the con- and reverter, requiring an IReverter
   * @param r a reverter which will be casted into a converter to fill necessary variables
   * @throws NoConverterFoundException if converter did not match converting requirements
   * @throws NoReverterFoundException if converter did not match reverting requirements
   */
  protected def setVerters(r : IReverter[T, O]) = {
    val aReverter = if (r._canRevert(from, to)) r else throw new NoReverterFoundException(from, to)
    val aConverter  = r match {
      case c : IConverter[_, _] if c._canConvert(from, to) => c.asInstanceOf[IConverter[T, O]]
      case _ => throw new NoConverterFoundException(from, to)
    }
    Some(aConverter -> aReverter)
  }

  /**
   * returns the name of the svars created by the conversion info which is used to inject the svar into an entity
   * @return the name of the svars created by the conversion info which is used to inject the svar into an entity  
   */
  def getSVarName =
    from.sVarIdentifier

  /**
   *  returns the reverter used by this conversioninfo. If it was not set before, a matching registered reverter
   * is set and returned 
   * @return the reverter used by this conversioninfo.
   */
  def accessReverter() : IReverter[T, O] = theReverter.getOrElse {
    theReverter = Some(Reverter(from, to))
    theReverter.get
  }

  /**
   *  returns the converter used by this conversioninfo. If it was not set before, a matching registered converter
   * is set and returned
   * @return the converter used by this conversioninfo.
   */
  def accessConverter() : IConverter[T, O] = theConverter.getOrElse {
    theConverter = Some(Converter(from, to))
    theConverter.get
  }

//  def bindReverter(svar : SVar[O])  : IReverter[T, O] =  accessReverter() match {
//    case b : BufferedConverter[T, O] => b.bind(svar)
//    case x => throw new InvalidConverterException
//  }
}

/**
 *  Representation of a conversion used to provide svars. Reads like provide a svar of type T as a svar of type O.
 * Actually creates a svar of type O, injects it to an entity and returns an adapter which looks like a svar of type T 
 */
class ProvideConversionInfo[T, O : ClassTag] private( val from : ConvertibleTrait[T],
                                                      val to : ConvertibleTrait[O],
                                                      val const : Boolean,
                                                      private val annotations : Set[Annotation],
                                                      val initialValue : Option[T],
                                                      verters : Option[(IConverter[T, O], IReverter[T, O])])
  extends ConversionInfo[T, O](verters) with Serializable
{
  def this(from : ConvertibleTrait[T], to : ConvertibleTrait[O], annotations : Set[Annotation]= Set()) =
    this(from, to, false, annotations, None, None)

  def asConst =
    new ProvideConversionInfo(from, to, true, annotations, initialValue, verters)

  /**
   *  sets the initial value
   *
   * @param value the initial value
   */
  def setInitialValue( value : T ) =
    new ProvideConversionInfo(from, to, const, annotations, Some(value), verters)

  def addAnnotations( as : Set[Annotation] ) =
    new ProvideConversionInfo(from, to, const, annotations ++ as, initialValue, verters)

  /**
   *  defines the converter to be used
   * @param c the converter to be used
   * @return the conversion info having the converter set
   */
  def using(c: IConverter[T, O]) : ProvideConversionInfo[T, O] =
    new ProvideConversionInfo(from, to, const, annotations, initialValue, setVerters(c))

  def makeSVal : SVal.SValType[O] =
    to apply accessConverter().convert( initialValue.getOrElse( throw new Exception) )

  /**
   *  creates and injects a svar into an entity. Uses the initial value if set, calls the constructor provided
   * with the from-convertible otherwise
   * @param e the entity into which the created svar should be injected
   * @return a tuple of a wrapped svar having the required type and the entity after injecting the svar
   */
  protected[core] def injectSVar( e : Entity, at : Time, bufferMode : BufferMode )(handler : Entity => Any)(implicit actorContext : SVarActor) {
    e.injectSVar(getSVarName, if (const) makeSVal else SVarImpl(makeSVal, at, bufferMode), to, annotations.toSeq :_*)( handler(_))
  }
}

// the conversion is kind of inverted, as the adapter has to do the same stuff as the provided version does
// in other words: even though the adapter looks like some output converter, it is an input converter
class RequireConversionInfo[O, T](val to : ConvertibleTrait[O], val from : ConvertibleTrait[T],
                                  verters : Option[(IConverter[T, O], IReverter[T, O])] = None)
  extends ConversionInfo[T, O] with Serializable {
  /**
   *  defines the reverter to be used
   * @param c the reverter to be used
   * @return the conversion info having the reverter set
   */
  def using(c: IReverter[T, O]) : RequireConversionInfo[O, T] =
    new RequireConversionInfo(to, from, setVerters(c))

}
