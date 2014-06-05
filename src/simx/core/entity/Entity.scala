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

package simx.core.entity

import simx.core.svaractor.{SVarActor, SimXMessage, SVar}
import description.GeneralEntityDescription
import simx.core.ontology.GroundedSymbol
import typeconversion._
import scala.Some


/* author: dwiebusch
* date: 27.08.2010
*/

/**
 *  the entity class
 */
class Entity protected[entity]( private val sVars : Map[Symbol, List[SVarContainer[_]]], val id : java.util.UUID )
  extends Serializable
{
  protected var removeObservers = Set[SVarActor.Ref]()
  /**
   *  Copy-C'tor
   */
  protected[core] def this( e : Entity ) =
    this(e.sVars, e.id)

  /**
   *  creates an empty entity
   */
  def this() =
    this(Map[Symbol, List[SVarContainer[_]]](), java.util.UUID.randomUUID)

  /**
   *  Returns the sVarNames of all SVars of this Entity
   */
  def getAllSVarNames =
    sVars.keySet

  def getAllSVars : Iterable[(Symbol, ConvertibleTrait[_], SVar[_])] =
    sVars.flatMap( x => x._2.map( y => (x._1, y.info, y.svar) ) )

  def contains[T]( typeInfo : TypeInfo[T] ) =
    sVars.contains(typeInfo.semantics.toSymbol)

  def execOnSVars[T, U]( info : ConvertibleTrait[T], annotations : GroundedSymbol* )( f : SVar[T] => U ){
    implicit val classTag = info.classTag
    get(info, annotations :_*).map(f.apply)
  }

  //for debugging purposes
  private var name : Option[String] = None

  def setName(n : String){
    name = Some(n)
  }

  def remove()(implicit actor : SVarActor.Ref) {
    GeneralEntityDescription.propagateEntityRemoval( this )
    removeObservers.foreach( _ ! RemoveEntityMessage(this ) )
  }

  def getRemoveObservers =
    removeObservers

  def setRemoveObservers(newSet : Set[SVarActor.Ref]){
    removeObservers = newSet
  }

  /**
   *  injects a svar into this entity
   * @param sVar the svar to be injected
   * @param info a convertible trait representing the svar's type
   * @return an entity containing the given svar
   */
  def injectSVar[T](sVar : SVar[T], info : ConvertibleTrait[T], annotations : GroundedSymbol*) : Entity =
    injectSVar(info.sVarIdentifier, sVar, info, annotations :_* )

  /**
   *  returns a svar retrieved by using the svaridentifier from the convertible trait. converts it to match the
   * given convertible trait
   * @param out the convertible trait specifiying the name and type of the svar to be returned
   * @return a list of svars matching the given convertible trait's description and annotations
   */
  def get[T](out : ConvertibleTrait[T], annotations : GroundedSymbol*) : List[SVar[T]] =
    accessFiltered(out, annotations :_*).map( _ convertSVar out )

  /**
   *  returns a svar retrieved by using the svaridentifier from the convertible trait. converts it to match the
   * given convertible trait. Using the given reverter is enforced
   * @param out the convertible trait specifiying the name and type of the svar to be returned
   * @param reverter the reverter to be used
   * @return a svar matching the given convertible trait's description
   */
  def get[T](out : ConvertibleTrait[T], reverter : IReverter[T, _], annotations : GroundedSymbol*) : List[SVar[T]] =
    accessFiltered(out, annotations :_*).map{ _.as(out, Some(reverter) ).svar }

//  def get[T, O](request : Require[O, T]) : List[SVar[T]] = {
//    val internal = request.wrapped.to
//    accessFiltered(internal).map(x => x.convertSVar(request.wrapped.from, Some(request.wrapped.bindReverter(x.svar))))
//  }

  def getAnnotationsFor[T]( c : ConvertibleTrait[T], annotations : GroundedSymbol* ) : List[Set[GroundedSymbol]] =
    accessFiltered(c, annotations :_*).map{ _.annotations }

  protected def accessFiltered[T]( out : ConvertibleTrait[T], annotations : GroundedSymbol*) : List[SVarContainer[T]]=
    sVars.getOrElse(out.sVarIdentifier, Nil).asInstanceOf[List[SVarContainer[T]]].filter{
      c => annotations.forall(c.annotations.contains)
    }

  protected def getSimpleName : String =
    getClass.getSimpleName

  //! returns nice string representation of this entity
  override def toString =
    if (sVars.isEmpty)
      getSimpleName + "(" + name.getOrElse(id) + ")"
    else
      getSimpleName + "("+ name.getOrElse(id) + ") with " + sVars.map{
        t => t._1.name + (if (t._2.size > 1 ) "("+t._2.size+"x)" else "")
      }

  //! redirects the hashCode method call to the UUIDs hashCode method
  override def hashCode =
    id.hashCode

  //! redirects the equals method call to the UUIDs equals method
  override def equals(obj: Any) = obj match {
    case that : Entity => that.id == id
    case _ => false
  }

  /**
   *  injects a svar into this entity
   * @param sVarName the symbol used to inject the svar
   * @param sVar the svar to be injected
   * @param info a convertible trait representing the svar's type
   * @return an entity containing the given svar
   */
  protected[entity] def injectSVar[T](sVarName : Symbol, sVar : SVar[T],
                                      info : ConvertibleTrait[T], annotations : GroundedSymbol*) : Entity =
    new Entity(sVars.updated(sVarName, SVarContainer(sVar, info, annotations.toSet) :: sVars.getOrElse(sVarName, Nil) ), id)
}

//! Some Exception which is thrown when the wrong converter was specified
class InvalidConverterException extends Exception
//! Some Exception which is thrown when the requested svar does not exist within an entity
class SVarDoesNotExistException( val name : Symbol ) extends Exception(name.toString())
//! message to inform an actor about the removal of an entity
protected[entity] case class RemoveEntityMessage( e : Entity )
                                                (implicit @transient actor : SVarActor.Ref) extends SimXMessage

/**
 * implementation of the SVarContainerBase trait
 * For internal use 
 */
protected case class SVarContainer[T]( svar : SVar[T], info : ConvertibleTrait[T], annotations : Set[GroundedSymbol] ){
  def as[O]( out : ConvertibleTrait[O], reverter : Option[IReverter[O, _]] = None) : SVarContainer[O] =
    SVarContainer(convertSVar(out, reverter), out, annotations)

  def convertSVar[O]( out : ConvertibleTrait[O], reverter : Option[IReverter[O, _]] = None) : SVar[O] =
    info.createConverter(svar, out, reverter)
}
