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

package simx.core.svaractor

import java.util.UUID
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag


object SVarImpl extends SVarObjectInterface {
  def apply[T](value: T)(implicit actorContext : SVarActor, typeTag : ClassTag[T]) : SVar[T] =
    actorContext.createSVar(value)(typeTag)
}

class SVarImpl[T : ClassTag] private(val initialOwner: SVarActor.Ref,
                                    val id: UUID,
                                    val containedValueManifest: ClassTag[T]) extends SVar[T] with Serializable {

  def this(initialOwner: SVarActor.Ref, man: ClassTag[T]) =
    this(initialOwner, UUID.randomUUID, man)

  def get(handler: (T) => Any)(implicit actorContext : SVarActor){
    actorContext.get(this)(handler)
  }

  def set(value: T)(implicit actorContext : SVarActor) {
    actorContext.set(this, value)
  }

  def observe(handler: (T) => Any)(implicit actorContext : SVarActor) {
    observe(handler, Set())
  }

  def observe(handler: (T) => Any, ignoredWriters : Set[SVarActor.Ref])(implicit actorContext : SVarActor) {
    actorContext.observe(this, ignoredWriters)(handler)
  }

  def ignore()(implicit actorContext : SVarActor) {
    actorContext.ignore(this)
  }

}
