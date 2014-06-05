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

package simx.core.component

import simx.core.entity.Entity
import simx.core.entity.component._
import simx.core.entity.typeconversion.{ConvertibleTrait, ProvideConversionInfo}
import simx.core.entity.description.{SValSet, EntityAspect}
import simx.core.ontology.{GroundedSymbol, types, Symbols}
import simx.core.component.remote.{RemoteActor, Start}
import simx.core.svaractor.SVarActor
import java.util.UUID

protected[core] trait ComponentCreation extends SVarActor with EntityCreationHandling with StackableEntityConfigLayer {
  private var openCreateRequests = Map[Entity, (SVarActor.Ref, List[ProvideConversionInfo[_ , _]])]()

  provideInitialValuesFor{
    case (toProvide, aspect, e, given) if aspect.semanticsEqual(Symbols.component) =>
      aspect match {
        case InnerComponentAspect(c : RemoteComponentAspect[_]) =>
            ask[SVarActor.Ref](RemoteActor.self, Start(c.nodeName.get, c)){ requestConfig(_, c, e) }
        case InnerComponentAspect(c : ComponentAspect[_]) =>
            requestConfig(createActor(c.props){ _ => {}}(_.printStackTrace()), c, e)
        case _ => throw new Exception("invalid aspect " + aspect)
      }
  }

  private def requestConfig(component : SVarActor.Ref, c : ComponentAspect[_], e : Entity){
    ask[SValSet](component, GetInitialConfigValuesMsg(UUID.randomUUID(), c, e)){ set =>
      val toCreate  = set.values.flatMap( _.map( _.asProvide.wrapped ) )

      if (toCreate.nonEmpty)
        openCreateRequests = openCreateRequests.updated(e, component -> toCreate.toList)

      provideInitialValues(e, SValSet( types.Component(component), types.Name(c.cName.name) ) )
    }
  }

  override protected def injectSVarCreation(entity : Entity) =
    if (openCreateRequests contains entity) openCreateRequests(entity) :: Nil else Nil

  protected def entityConfigComplete(e: Entity, aspect: EntityAspect){
    openCreateRequests = openCreateRequests - e
  }

  protected def configure(params: SValSet){}
  protected def removeFromLocalRep(e: Entity){}
  protected def requestInitialConfigValues( toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity) =
    SValSet()
}
