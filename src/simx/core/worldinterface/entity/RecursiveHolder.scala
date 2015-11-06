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

package simx.core.worldinterface.entity

import simx.core.entity.Entity

import scala.collection.mutable

/**
 * @author Dennis Wiebusch
 */
private[worldinterface] class RecursiveHolder{
  val items = mutable.Map[Symbol, Entity]()
  val children = mutable.Map[Symbol, RecursiveHolder]()

  def getItemsRecursively : Set[Entity] =
    children.foldLeft(items.values.toSet)( _ ++ _._2.getItemsRecursively )

  def flatten : Set[(Entity, List[Symbol])] = {
    val flatChilds = children.flatMap{ tuple => tuple._2.flatten.map( t => t._1 -> (tuple._1 :: t._2 ) ) }
    flatChilds.toSet ++ items.map(tuple => tuple._2 -> List(tuple._1))
  }

  def remove(e : Entity) {
    items.retain( (_, b) => b != e )
    children.values.foreach( _.remove(e) )
  }
}