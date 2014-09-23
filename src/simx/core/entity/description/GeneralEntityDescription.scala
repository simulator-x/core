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

package simx.core.entity.description

import collection.mutable
import scala.language.existentials
import simx.core.entity.component._
import simx.core.entity.typeconversion._
import java.lang.Exception
import simx.core.entity.Entity
import simx.core.svaractor.SVarActor


/**
 * @author dwiebusch
 * Date: 09.05.11
 * Time: 09:32
 */

class GeneralEntityDescription[+Type <: Entity, B <: Type] protected (val typeDef     : TypeInfo[Type, B],
                                                           val createType  : (Entity, SVarActor)  => Type,
                                                           theHandler      : Option[Type  => Any],
                                                           val path        : List[Symbol],
                                                           val aspects     : Seq[EntityAspect]) extends Serializable {
  //initially check validity of this description
  initialCheck(aspects)

  def realize(handler : Type => Any = _ => () )( implicit entityCreationContext : EntityCreationHandling ){
    entityCreationContext.realize(this)(handler)
  }

  def copy(newAspects : Seq[EntityAspect] = aspects) =
    new GeneralEntityDescription[Type,  B](typeDef, createType, theHandler, path, newAspects)

  def createSVal(that : B) = typeDef.asConvertibleTrait.apply(that)

  /**
   *  checks the description fort validity. Throws an InvalidEntityDescriptionException if it's not
   */
  private def initialCheck( eAspects : Seq[EntityAspect] ) {
    val provideMap        = mutable.Map[ConvertibleTrait[_], Set[EntityAspect]]()
    val ownerMap          = mutable.Map[ConvertibleTrait[_], Set[EntityAspect]]()
    val ProvideOverrides  = mutable.Map[ConvertibleTrait[_], EntityAspect]()
    val OwnerOverrides    = mutable.Map[ConvertibleTrait[_], EntityAspect]()
    val Features          = eAspects.flatMap(_.getFeatures).toSet
    val Providings        = eAspects.flatMap(_.getProvidings).toSet
    val missingProvidings = Features -- Providings
    val doCoreCheck       = !System.getProperty("disableSafetyChecks", "false").toBoolean
    var nonCoreTypes      = if (doCoreCheck) (Features ++ Providings).filterNot(_.isCoreType) else Set[ConvertibleTrait[_]]()

    //store overrides
    val multipleOverrides = eAspects.foldLeft(""){ (str, aspect) =>
      aspect.overrides.foldLeft(str){ (s, overr) =>
        overr match {
          case Provide(c) if ProvideOverrides.getOrElseUpdate(c.from, aspect) != aspect =>
            s + "\t" + c.from + " shall be provided by " + ProvideOverrides(c.from).componentType +
              " and " +  aspect.componentType + "\n"
          case Own(c) if OwnerOverrides.getOrElseUpdate(c, aspect) != aspect =>
            s + "\t" + c + " shall be owned by " + OwnerOverrides(c).componentType
            " and " + aspect.componentType + "\n"
          case _ => s + ""
        }
      }
    }

    //check for invalid assertions
    val invalidAssertions = for (asp <- eAspects) yield {
      //check if providings is a subset of features
      val invalidProvidings = asp.getProvidings.filterNot(asp.getFeatures.contains)
      //check for invalid overrides
      val invalidProvides   = mutable.Set[ConvertibleTrait[_]]()
      val invalidOwnerships = mutable.Set[ConvertibleTrait[_]]()

      asp.overrides.foreach{
        case Provide(c) =>
          if ( doCoreCheck && !c.from.isCoreType )
            nonCoreTypes = nonCoreTypes + c.from
          if (ProvideOverrides.contains(c.from))
            provideMap.update(c.from, Set(ProvideOverrides(c.from)))
          else
            provideMap.update(c.from, provideMap.getOrElseUpdate(c.from, Set()) + asp)
          if (!asp.getProvidings.contains(c.from))
            invalidProvides += c.from
        case Own(c) =>
          if (!c.isCoreType)
            nonCoreTypes = nonCoreTypes + c
          if (OwnerOverrides.contains(c))
            ownerMap.update(c, Set(OwnerOverrides(c)))
          else
            ownerMap.update(c, ownerMap.getOrElseUpdate(c, Set()) + asp)
          if (!Features.contains(c))
            invalidOwnerships += c
        case _ =>
      }
      (asp, invalidProvidings, invalidProvides, invalidOwnerships)
    }

    //create exceptionText
    var exceptionText = if (multipleOverrides.isEmpty) "" else "Multiple overridings:\n" + multipleOverrides
    for (mp <- missingProvidings) exceptionText += "\t" + mp + " is never provided\n"
    for ((c, owners) <- ownerMap)
      if (owners.size > 1)
        exceptionText += "\t" + c + " shall be owned by multiple aspects: " + owners + "\n"
    for ((c, providers) <- provideMap)
      if (providers.size > 1)
        exceptionText += "\t" + c + " shall be provided by multiple aspects: " + providers + "\n"
    for (nonCoreType <- nonCoreTypes)
      exceptionText += "\tUsed specific type " + nonCoreType + "  from " + nonCoreType.getClass.getPackage + ". Use type from simx.core.ontology.types instead\n"
    for (ia <- invalidAssertions) {
      ia._2.foreach( c => exceptionText += "\t" + c + " in " + ia._1 + " shall be provided but is not included in the feature list\n" )
      ia._3.foreach( c => exceptionText += "\t" + c + " in " + ia._1 + " shall be provided but is not included in the providings list\n" )
      ia._4.foreach( c => exceptionText += "\t" + c + " in " + ia._1 + " shall be owned but is not provided anywhere\n" )
    }
    if (exceptionText.nonEmpty)
      throw InvalidEntityDescriptionException( this, exceptionText )
  }
}

case class ResolveRequirementsException( aspects : List[EntityAspect] )
  extends Exception("Could not resolve requirements for aspects:\n\t" + aspects.mkString("\n\t"))

case class DoubleDefinitionException( doubles : String )
  extends Exception("The following SVars are at least defined twice:\n\t" + doubles)

case class InvalidEntityDescriptionException[T <: Entity]( ed : GeneralEntityDescription[T, _ <: T], reason : String )
  extends Exception("Invalid EntityDescription " + ed + ":\n" + reason)

case class NoInitialValuesException(m : Seq[TypeInfo[_, _]], ownerMap : collection.mutable.Map[Symbol, SVarActor.Ref], as : Seq[EntityAspect])
  extends Exception( m.foldLeft("The following values are never provided:"){
    (str, elem) => str + "\n\t" + elem + " (owner: " + ownerMap.get(elem.sVarIdentifier).collect{
//      case c : Component => c.componentName.name + ", semantics: " +
//        as.find(_.componentType.equals(c.componentType)).collect{
//          case a => a.getCreateParams.semantics.toString
//        }.getOrElse("unknown")
      case a => a.toString()
    }.getOrElse("unknown") +")"
  } )