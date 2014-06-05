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

package simx.core.worldinterface.eventhandling

import simx.core.ontology.GroundedSymbol
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.description.{SValSet, SVal}
import simx.core.entity.Entity

/* author: dwiebusch
 * date: 03.12.2010
 */

//TODO Immutable
class Event protected[eventhandling](
  val name : GroundedSymbol,
  val values : SValSet = new SValSet(),
  val affectedEntities : Set[Entity] = Set() )
extends Serializable {
  def get[T]( c : ConvertibleTrait[T] ) : Option[T] =
    values.getFirstValueFor(c)

  def getAll[T]( c : ConvertibleTrait[T] ) : List[T] =
    values.getAllValuesFor(c)

  override def toString: String =
    "Event (" + name.value + " with " + values + "\n)"
}

case class EventDescription(
  name : GroundedSymbol,
  hasToContain: List[ConvertibleTrait[_]] = Nil,
  restriction : Option[PartialFunction[Event, Boolean]] = None )
{
  def matches( e : Event ) : Boolean =
    if (e.name equals name) restriction collect { case f => f(e) } getOrElse true else false

  def createEvent(affectedEntities : Set[Entity], values : SVal[_]* ) : Event = {
    //TODO Check Values
    new Event(name, new SValSet(values:_*), affectedEntities)
  }

  def createEvent( values : SVal[_]* ) =
    apply(values :_*)

  def apply(values : SVal[_]* ) : Event =
    apply(Set[Entity](), values:_*)

  def apply( affectedEntities : Set[Entity], values : SVal[_]* ) : Event =
    createEvent(affectedEntities, values:_*)

  def restrictedBy(_restriction: PartialFunction[Event, Boolean]) : EventDescription =
    new EventDescription(name, hasToContain, Some(_restriction))

  override def hashCode() =
    name.hashCode()

  override def equals(that: Any) = that match {
    case desc : EventDescription => desc.name.equals(name)
    case _ => false
  }
}

object DefaultRestrictions{
  def containsEntity(ent : Entity) : PartialFunction[Event, Boolean] = {
    case (e : Event) => e.affectedEntities.contains(ent)
  }

  def containsEntities(set : Set[Entity]) : PartialFunction[Event, Boolean] = {
    case (e : Event) => set.forall( e.affectedEntities.contains )
  }
}