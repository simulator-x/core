/*
 * Copyright 2015 The SIRIS Project
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

package simx.core.svaractor.semantictrait.example

import simx.core.ontology.types.{Scale, Position, Gravity, Position2D}
import simx.core.svaractor.SVarActor
import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.semantictrait.example.relations._
import simx.core.svaractor.semantictrait.base.{SemanticTrait, SpecificSemanticTrait}
import simx.core.svaractor.semantictrait.example.types._
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

/**
 *
 * Created by dennis on 23.01.15.
 */
package object traits {
  object Movable extends SpecificSemanticTrait(Gravity :: Anything){
    final type SpecificEntityType = SemanticEntity{ def moveTo(s : Location.ValueType)(implicit creator : EntityUpdateHandling)  }

    override protected def createEntity(e: SemanticEntity.ValueType)(implicit creator : SVarActor) = new SemanticEntity(e) {
      def moveTo(s : Location.ValueType)(implicit creator : EntityUpdateHandling){
        println("Entity#" + entity.value.id + " is now in/at " + s.value)
      }
    }
  }

  object Vehicle extends SpecificSemanticTrait( Position2D :: Scale, has(SteeringBehavior), has(4 < _, Wheel) ){
    final type SpecificEntityType = SemanticEntity{ def moveTo(s : Location.ValueType)(implicit creator : EntityUpdateHandling) : Unit@CPSRet  }

    protected def createEntity(e: SemanticEntity.ValueType)(implicit creator : SVarActor) = new SemanticEntity(e) {
      def moveTo(s : Location.ValueType)(implicit creator : EntityUpdateHandling) : Unit@CPSRet ={
        (entity modify s).apply
        println("entity " + entity + " is now in/at " + (entity get s))
      }
    }
  }

  object Wheel extends SemanticTrait(Position :: Scale, has(Shape("round")))
}
