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

package simx.core.entity

import scala.reflect.ClassTag
import simx.core.svaractor.{StateParticle, SVarActor}
import simx.core.worldinterface.WorldInterfaceHandling
import simx.core.worldinterface.eventhandling.{EventDescription, Event, EventHandler, EventProvider}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.ontology.Symbols
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

/**
 * Created by dwiebusch on 23.12.13
 */
object EntityBehavior{
  final class Attachable[T <: BaseBehavior : ClassTag](behavior : => T){
    def to(e : Entity)(implicit context : SVarActor.Ref) : SVarActor.Ref =
      SVarActor.createActor(behavior)
  }

  def attachBehaviour[T <: BaseBehavior : ClassTag](behavior : => T) =
    new Attachable(behavior)
}

abstract class BaseBehavior(entity : Entity) extends SVarActor with EventProvider with EventHandler with WorldInterfaceHandling with EntityUpdateHandling{
  private var valueMap = Map[ConvertibleTrait[_], Any]()

  private val valueChangeEvent = new EventDescription(Symbols.value)


  def observe[T](c : ConvertibleTrait[T])(handler : T => Any ){
    entity.observe(c).head{
      newValue  =>
        handler(newValue)
        valueMap = valueMap.updated(c, newValue)
    }
  }

  def getValue[T](c : ConvertibleTrait[T]) : Option[T] = {
    observe(c)((_ : T) => {})
    valueMap.get(c) collect { case x : T @unchecked => x }
  }

  override def shutdown(){
//    entity.getAllSVars.foreach(_._2._2.ignore())
    //TODO: FIXME
    super.shutdown()
  }
}
