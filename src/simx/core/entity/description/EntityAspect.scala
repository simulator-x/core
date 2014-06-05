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

package simx.core.entity.description

import simx.core.ontology.GroundedSymbol
import simx.core.entity.typeconversion._
import simx.core.entity.Entity

/**
 * Base class for Aspects
 * @param componentType The type of the component which this aspect shall be sent to
 * @param aspectType The type of this aspect. Used for identification of aspects within the respective component's
 *                   requestInitialValues and entityConfigComplete methods
 * @param targets The names of the components which will receive this aspect. If empty all components of the type componentType
 * @param aspectId An identifier for this aspect
 */
abstract class EntityAspect private (val componentType : GroundedSymbol,
                                     val aspectType    : GroundedSymbol,
                                     val targets       : List[Symbol],
                                     val aspectId      : java.util.UUID)
  extends Serializable {
  /**
   * Base class for Aspects
   * @param componentType The type of the component which this aspect shall be sent to
   * @param aspectType The type of this aspect. Used for identification of aspects within the respective component's
   *                   requestInitialValues and entityConfigComplete methods
   * @param targets The names of the components which will receive this aspect. If empty all components of the type componentType
   */
  def this( componentType : GroundedSymbol,aspectType : GroundedSymbol, targets : List[Symbol] = Nil ) =
    this (componentType, aspectType, targets, java.util.UUID.randomUUID())

  /**
   * the features the entity will at least have when it is created
   * @return the features the entity will at least have when it is created
   */
  def getFeatures     : Set[ConvertibleTrait[_]]

  /**
   * the features the component for which this aspect is designed *must* provide (i.e. provide initial values for those
   * features)
   * @return a set of features
   */
  def getProvidings   : Set[ConvertibleTrait[_]]

  /**
   *
   * The list of create parameters
   * @return a list of [[simx.core.entity.description.SVal]]'s containing the information needed to instanciate an
   *         entity with this aspect
   */
  def getCreateParams : NamedSValSet

  /**
   * Accesses the set of dependencies which is generated by this aspect
   * @return a set of [[simx.core.entity.description.Dependencies]]
   */
  def getDependencies : Set[Dependencies] =
    Set(Dependencies(Providing(getProvidings.toSeq :_*), Requiring(getRequirings.toSeq :_*)))

  /**
   * Accesses the [[simx.core.entity.typeconversion.Provide]]s and [[simx.core.entity.typeconversion.Require]]s which
   * are enforced for this aspect
   * @return a sequence of [[simx.core.entity.typeconversion.ProvideAndRequire]]s
   */
  def overrides : Seq[ProvideAndRequire] =
    Seq()

  /** the parent element for this aspect */
  @transient private var parentElement : Option[Entity] = None

  /**
   * Sets the parent element for this aspect
   * @param p the new parent element
   */
  def setParent( p : Entity ){
    parentElement = Some(p)
  }

  /**
   * Checks if a parent element was defined for this aspect
   * @return true if a parent element was defined, false otherwise
   */
  def hasParent =
    parentElement.isDefined

  /**
   * accesses the parent element
   * @note check if a parent was set using the hasParent method before calling this method
   * @return the parent element of this aspect
   */
  def parent =
    parentElement.get

  /**
   * Accesses the features which have to be provided by other components
   * @return a set of the features which have to be provided by other components
   */
  final def getRequirings : Set[ConvertibleTrait[_]] =
    getFeatures -- getProvidings

  /**
   * creates a clone of this EntityAspect with some enforced providings and requirings
   * @param newOverrides the new enforced providings and requirings
   * @return a clone of this EntityAspect
   */
  def where( newOverrides : ProvideAndRequire* ) : EntityAspect =
    getClone(targets, newOverrides)

  /**
   * creates a clone of this EntityAspect whith for the given targets
   * @param trgts the new targets
   * @return a clone of this EntityAspect
   */
  def forTargets( trgts : List[Symbol] ) : EntityAspect =
    getClone(trgts, overrides)

  private def getClone(newTargets : List[Symbol], newOverrides : Seq[ProvideAndRequire] ) = {
    val features     = getFeatures
    val providings   = getProvidings
    val createParams = getCreateParams
    new EntityAspect(componentType, aspectType, newTargets, aspectId) {
      override def overrides = newOverrides
      def getCreateParams    = createParams
      def getProvidings      = providings
      def getFeatures        = features
    }
  }

  /**
   * a print bit more information on this aspect
   * @return a string containing information on this aspect
   */
  override def toString = "EntityAspect " + getCreateParams.semantics.value.toString +
    " for Components of type " + componentType.value.toSymbol.name +
    (if (targets.isEmpty) "" else " for targets: " + targets)


  //equals
  def semanticsEqual( semantics : GroundedSymbol ) : Boolean =
    getCreateParams.semantics.equals(semantics)

  override def hashCode() = aspectId.hashCode()

  override def equals(p1: Any) = p1 match {
    case that : EntityAspect => aspectId.equals(that.aspectId)
    case _ => false
  }

  /**
   * wrapper to make cvar adding easier
   * @param tuples the set of create parameters to be added
   * @return a list of create parameters
   */
  protected def addCVars( tuples : => TraversableOnce[SVal[_]] ) : NamedSValSet =
    tuples.foldLeft(new NamedSValSet(aspectType))( (cps, cvar) => cps += cvar )

  /**
   * wrapper to make cvar adding easier
   * @param value the create parameter to be added
   * @return a list of create parameters
   */
  protected def addCVar( value : SVal[_] ) : NamedSValSet =
    new NamedSValSet(aspectType, value)
}
