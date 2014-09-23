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

package simx.core.svaractor.unifiedaccess

import simx.core.svaractor.SVarActor
import simx.core.svaractor.TimedRingBuffer.{Time, ContentType}

/**
 * Created by dwiebusch on 04.03.14
 */
trait Observability[+T] extends Serializable {
  final def observe(handler : T => Unit, ignoredWriters : Set[SVarActor.Ref])(implicit actorContext : SVarActor) : java.util.UUID =
    observe((x : T, _) => handler(x), ignoredWriters)

  final def observe(handler : T => Unit)(implicit actorContext : SVarActor) : java.util.UUID =
    observe(handler, Set[SVarActor.Ref]())

  def observe(handler : (T, Time) => Unit, ignoredWriters : Set[SVarActor.Ref])(implicit actorContext : SVarActor) : java.util.UUID


  final private[unifiedaccess] def observe(ignoredWriters : Set[SVarActor.Ref])(handler : ContentType[T] => Unit, actorContext : SVarActor){
    observe((value, time) => handler(value -> time), ignoredWriters)(actorContext)
  }
}