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

package simx.core.svaractor.semantictrait.base

import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

/**
 * Created by dwiebusch on 30.11.14
 */
class Relation[T <: Semantic.Value[_], U <: Semantic.Value[_], S <: Thing](val valueDescription : ValueDescription[_ <: Base, S],
                                                                                           val value : (T, U))
  extends SemanticValue[(T, U), S]
{
  override def toString: String =
    "(" + value._1 + " " + valueDescription + " " + value._2 + ")"


  override def subsumes(that: Semantic.Value[_])(implicit context: EntityUpdateHandling): Boolean@CPSRet = {
    if(valueDescription subsumes that.valueDescription){
      val (thatSubj, thatObj) = that.value.asInstanceOf[(Semantic.Value[_], Semantic.Value[_])]
      value._1.subsumes(thatSubj) && value._2.subsumes(thatObj)
    } else
      false
  }
}
