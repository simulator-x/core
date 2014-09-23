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

package simx.core.helper

import java.lang.ref.{WeakReference, ReferenceQueue}
import java.util.UUID
import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

/**
 * Created by dwiebusch on 11.09.14
 */
object GarbageCollectionObserver {
  def observe(garbageObject : Any) {
    if (garbageObject == null)
      throw new NullPointerException()

    val key = UUID.randomUUID()
    map.put(key, new GarbageReference(garbageObject, key, garbageObject.toString, map))
  }


  private val map = new ConcurrentHashMap[UUID, GarbageReference]()
  private val referenceQueue = new ReferenceQueue[Any]()
  CleanupThread.start()

  private class GarbageReference(referent: Any, val key: UUID, val name: String, val map: ConcurrentMap[UUID, GarbageReference])
    extends WeakReference[Any](referent, referenceQueue) {
  }

  private object CleanupThread extends Thread {
    setPriority(Thread.MAX_PRIORITY)
    setName("GarbageCollectionObserver-cleanupthread")
    setDaemon(true)

    override def run() {
      while (true) {
        try {
          var ref = referenceQueue.remove().asInstanceOf[GarbageReference]
          while (true) {
            System.out.println("gc removing " + ref.name)
            ref.map.remove(ref.key)
            ref = referenceQueue.remove().asInstanceOf[GarbageReference]
          }
        } catch {
          case _: Throwable => //ignore
        }
      }
    }
  }
}


