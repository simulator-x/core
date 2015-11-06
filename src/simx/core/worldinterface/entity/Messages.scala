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
import simx.core.svaractor.{SimXMessage, SVarActor}
import simx.core.worldinterface.ForwardableMessage

import scala.annotation.meta.param

private[core] case class CreationMessage ( path : List[Symbol], e : Entity )

/**
 * Message sent to the WorldInterface Actor,  to look up a specific entity
 * FOR INTERNAL USAGE ONLY
 *
 * @author dwiebusch
 * @param name the id of the entity to be looked up
 */
//private case class EntityLookupRequest(name : List[Symbol], future : SyncVar[Option[Entity]])
//private case class EntityGroupLookupRequest(name : List[Symbol], future : SyncVar[Option[Set[Entity]]])
private[worldinterface] case class LookupEntity(name : List[Symbol])
private[worldinterface] case class LookupEntities(name : List[Symbol])
private[worldinterface] case class AddEntityRegistrationListener( actor : SVarActor.Ref, path : List[Symbol] )
private[worldinterface] case class OnOneRegistration( path : List[Symbol])
private[worldinterface] case class OnNextRegistration( path : List[Symbol])

/**
 * Message sent to the WorldInterface Actor, to register an existing entity
 *
 * @author dwiebusch
 * @param name the name under which the entity will be accessible after being registered
 * @param e the entity to be registered
 */
private[worldinterface] case class EntityRegisterRequest(name : List[Symbol], e : Entity)(@(transient @param) implicit val sentBy : SVarActor.Ref)
  extends SimXMessage with ForwardableMessage{ protected def copy = EntityRegisterRequest(name, e)(sentBy) }

private[worldinterface] case class EntityUnregisterRequest(e : Entity)(implicit val sentBy : SVarActor.Ref)
  extends SimXMessage with ForwardableMessage{ protected def copy = EntityUnregisterRequest(e)(sentBy) }