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

package simx.core.components.io

import simx.core.ontology.types
import simx.core.entity.Entity
import simx.core.entity.description.SVal
import simx.core.worldinterface.WorldInterfaceHandling

/**
 * @author dwiebusch
 * Date: 17.10.11
 * Time: 14:45
 */

trait IODeviceProvider extends WorldInterfaceHandling{
  final protected def publishDevice[T <: Entity]( device : SVal[T], path : List[Symbol] = Nil){
    registerEntity(types.OntologySymbol(device.typedSemantics.semantics).value.toSymbol :: path, device.value)
  }
}