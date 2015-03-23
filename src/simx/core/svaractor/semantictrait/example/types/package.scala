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


import simx.core.ontology.Symbols
import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.semantictrait.Castability
import simx.core.svaractor.semantictrait.base.Semantic._
import simx.core.svaractor.semantictrait.base._
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

/**
 *
 * Created by dennis on 23.01.15.
 */
package object types {
  case object Anything extends SemanticType(classOf[Float], BaseValueDescription(symbols.any), true)
  case object Location extends SemanticType(classOf[String], BaseValueDescription(symbols.location), true)
  case object Number extends SemanticType(classOf[Int], BaseValueDescription(symbols.number), true)
  case object Integer extends SemanticType(classOf[Int], Number.valueDescription as symbols.integer, true)
  case object SteeringBehavior extends SemanticType(classOf[Int], BaseValueDescription(symbols.steering), true)
  case object Shape extends SemanticType(classOf[String], BaseValueDescription(symbols.shape), true)

  case object SemanticEntity extends SemanticType(classOf[simx.core.entity.Entity], BaseValueDescription(Symbols.entity)) {
    self =>

    override def apply(v : SemanticEntity.DataType) : SemanticEntity.ValueType with Castability  =
      new ValueType with Castability{
        override def attain(rel : Value[_]*)(implicit context: EntityUpdateHandling) = {
          super.attain(rel.map{
            case x : PartialRelation[SemanticEntity.DataType, _, _ ]@unchecked => x.apply(this)
            case y => y
          } :_*)
        }

        override def attainR[T, B <: Base, S <: Thing](retTyp : simx.core.svaractor.semantictrait.base.SemanticTypeTrait[T, B, S],
                                                       v :  Semantic.Value[_]*)(implicit context: EntityUpdateHandling) : Option[SemanticValue[T, S]]@CPSRet = {
          super.attainR(retTyp, v.map{
            case x : PartialRelation[SemanticEntity.DataType, _, _ ]@unchecked => x.apply(this)
            case y => y
          } :_* )
        }

        val valueDescription  = self.valueDescription
        val value: SemanticEntity.DataType = v
      }

    def createEntity(e : SemanticEntity.DataType) : SemanticEntity.ValueType with Castability =
      apply(e)
  }

}



