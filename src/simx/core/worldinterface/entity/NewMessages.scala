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

import java.util.UUID

import simx.core.entity.Entity
import simx.core.svaractor.SVarActor
import simx.core.worldinterface.entity.filter.EntityFilter

/**
 * Created by martin 
 * on 21/04/15.
 */
private[worldinterface] case class EntityAppearance(e : Entity, reference: UUID)
private[worldinterface] case class RequestEntities(filter: EntityFilter)
private[worldinterface] case class AddOnEntityAppearanceListener(actor : SVarActor.Ref, filter: EntityFilter, reference: UUID = UUID.randomUUID())
private[worldinterface] case class OnOneEntityAppearance(filter: EntityFilter)
private[worldinterface] case class OnNextEntityAppearance(filter: EntityFilter)