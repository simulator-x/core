/*
 * Copyright 2014 The SIRIS Project
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

package simx.core.helper

import java.util.UUID

import simx.core.entity.Entity
import simx.core.entity.component.EntityCreationHandling
import simx.core.entity.description.SVal.SValType
import simx.core.entity.description.{SVal, SValSet}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.ontology.EntitySVarDescription
import simx.core.svaractor.unifiedaccess.StateParticleInfo
import simx.core.svaractor.SVarActor
import simx.core.worldinterface.CreationMessage

import scala.collection.mutable

abstract class AllOrNothing[+T]
case object Nothing extends AllOrNothing[scala.Nothing]
case object All extends AllOrNothing[scala.Nothing]
case class SomeThing[+T](value: T) extends AllOrNothing[T]

/**
 *
 * Created by chrisz on 21/08/14.
 */
trait AutomaticLocalEntityRepresentation extends SVarActor with EntityCreationHandling with EntityObservation {

  private val worldState = collection.mutable.Map[UUID, SValSet]()
  private val entityIdentifier = collection.mutable.Map[String, mutable.Set[UUID]]()

  //if relevantEntityTypes == empty -> all Entities will be fetched and stored locally in your component
  protected val entityTypesForLocalRepresentation: AllOrNothing[List[EntitySVarDescription[_]]]
  protected def particleTypesForLocalRepresentation: AllOrNothing[List[ConvertibleTrait[_]]] = All

  registerForCreationOf(Nil)
  addHandler[CreationMessage]{ msg => handleCreation(msg.e) }
  handleRegisteredEntities(Nil){ _.map( e => handleCreation(e)) }

  private def handleCreation(e: Entity){
    e.addRemoveObserver(self)()

    var isContained = false
    entityTypesForLocalRepresentation match {
      case All =>
        isContained = true
      case SomeThing(entTypes) =>
        entTypes.foreach(x => if(x >@ e.description.typeDef) isContained = true)
      case Nothing =>
    }

    if(isContained) {
      handleNewEntity(e)
      val entID = e.id
      val entName = e.getSimpleName
      entityIdentifier.get(entName) match {
        case Some(list) => list.add(entID)
        case None => entityIdentifier.update(entName, mutable.Set[UUID](entID))
      }

      if (!worldState.contains(entID))
        worldState.update(entID, new SValSet())

      onEntityUpdate(e){
        case add : Add[_] => handleAdd(add, entID, entName)
        case Remove(entity, info) => handleRemove(entity, info)
      }
    }
  }

  private def handleAdd[T](add : Add[T], entID : UUID, entName : String){
    val doIt = particleTypesForLocalRepresentation match {
      case All => true
      case SomeThing(list) if list.exists(x => (x.sVarIdentifier == add.info.identifier) && add.info.typeInfo.isSubtypeOf(x)) => true
      case _ => false
    }
    if (doIt)
      add.info.svar.observe{ newVal =>
        val currentSet = worldState.get(entID).get
        val sVal = add.info.typeInfo(newVal)
        //println("Updating value of Type: " + sValType + " from Entity: " + entName + " with Value: " + newVal)
        val list = sVal :: currentSet.getOrElse(add.info.identifier, Nil).filterNot(_.typedSemantics.annotations.equals(add.info.annotations))
        currentSet += (add.info.identifier -> list)
        onUpdate(add.e, add.info.identifier, Some(sVal), isRemove = false)
        //replaceAllWith(sValType.apply(newVal.asInstanceOf[quadrupel._4.dataType]))
      }
  }

  private def handleRemove[T](entity : Entity, info : StateParticleInfo[T]) {
    onUpdate(entity, info.identifier, if (info.svar.isMutable) None else Some(info.svar.asInstanceOf[SValType[T]]), isRemove = true)
  }

  protected def onUpdate[T](entity : Entity, sVarIdentifier : Symbol, sval : Option[SVal.SValType[T]], isRemove : Boolean){
  }

  protected def handleNewEntity(e : Entity){
  }

  protected def getLocalEntityData(e: Entity) =
    worldState.getOrElse(e.id, SValSet())

  protected def getLocalEntityData(entName: String) = {
    var result: List[SValSet] = Nil
    entityIdentifier.get(entName).collect{
      case someIDs =>
        someIDs.foreach(id =>
          worldState.get(id).collect{case someSet => result = someSet :: result }
        )
    }
    result
  }
}
