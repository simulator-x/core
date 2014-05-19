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

package simx.core.component

import simx.core.svaractor.SVar
import simx.core.entity.Entity
import simx.core.entity.description.Semantics
import simx.core.ontology.GroundedSymbol
import simx.core.entity.description.NamedSValSet

/**
 * Created by IntelliJ IDEA.
 * User: dwiebusch
 * Date: 11.02.11
 * Time: 14:23
 */

abstract class ComponentFeature( val name : GroundedSymbol) {
  def svarHandling : (Symbol, SVar[_], Entity, NamedSValSet, Component) => Unit
  def entityHandling : (Entity, NamedSValSet, Component) => Unit
}

trait FeatureAdding extends Component{
  protected var featureMap = Map[GroundedSymbol, ComponentFeature]()

  protected def entityConfigComplete(e: Entity, cParam: NamedSValSet) =
    featureMap.get(cParam.semantics).collect{
      case feature => feature.entityHandling(e, cParam, this)
    }

  protected def handleNewSVar[T](sVarName: Symbol, sVar: SVar[T], e: Entity, cparam: NamedSValSet) =
    featureMap.get(cparam.semantics).collect{
      case feature => feature.svarHandling(sVarName, sVar, e, cparam, this)
    }

  def addFeature( f : ComponentFeature ) {
    featureMap = featureMap + (f.name -> f)
  }

  def removeFeature( name : Semantics ) {
    featureMap = featureMap.filterNot( _._1 == name )
  }
}

