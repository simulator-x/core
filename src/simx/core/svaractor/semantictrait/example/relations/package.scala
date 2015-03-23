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

import simx.core.entity.description.SVal.SValType
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.svaractor.semantictrait.base.RelationDescription
import simx.core.svaractor.semantictrait.base.Semantic.Value
import simx.core.svaractor.semantictrait.example.types.SemanticEntity

/**
 *
 * Created by dennis on 23.01.15.
 */
package object relations {
  case object in extends RelationDescription(classOf[Any], classOf[Any], symbols.in)
  case object affectedBy extends RelationDescription(classOf[Any], classOf[Any], symbols.affectedBy)

  case object has extends RelationDescription(classOf[SemanticEntity.DataType], classOf[Any], symbols.has)
  case object at extends RelationDescription(classOf[SemanticEntity.DataType], classOf[Any], symbols.at)
}
