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

package simx.core.worldinterface.entity.filter

import simx.core.entity.Entity
import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

/**
 * Created by martin 
 * on 24/04/15.
 */
abstract class EntityFilter extends (Entity => Boolean@CPSRet){
  outerEntityFilter =>

  implicit def actorContext: EntityUpdateHandling = _actorContext
  protected def Result = actorContext.Result

  private[worldinterface] def bindContext(ac: EntityUpdateHandling) = {
    _actorContext = ac
    this
  }
  private var _actorContext: EntityUpdateHandling = null

  def and(that: EntityFilter) = new EntityFilter {
    override private[worldinterface] def bindContext(ac: EntityUpdateHandling): EntityFilter = {
      outerEntityFilter.bindContext(ac)
      that.bindContext(ac)
      super.bindContext(ac)
    }
    override def apply(v1: Entity): Boolean@CPSRet = outerEntityFilter.apply(v1) && that.apply(v1)

    override def toString(): String = "(" + outerEntityFilter.toString() + " and " + that.toString() + ")"
  }

  def or(that: EntityFilter) = new EntityFilter {
    override private[worldinterface] def bindContext(ac: EntityUpdateHandling): EntityFilter = {
      outerEntityFilter.bindContext(ac)
      that.bindContext(ac)
      super.bindContext(ac)
    }
    override def apply(v1: Entity): Boolean@CPSRet = outerEntityFilter.apply(v1) || that.apply(v1)

    override def toString(): String = "(" + outerEntityFilter.toString() + " or " + that.toString() + ")"
  }

  def unary_! = new EntityFilter {
    override private[worldinterface] def bindContext(ac: EntityUpdateHandling): EntityFilter = {
      outerEntityFilter.bindContext(ac)
      super.bindContext(ac)
    }
    override def apply(v1: Entity): Boolean@CPSRet = !outerEntityFilter.apply(v1)

    override def toString(): String = "(not " + outerEntityFilter.toString() + ")"
  }

}
