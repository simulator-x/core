/*
 * Copyright 2016 The SIRIS Project
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

import simx.core.svaractor.handlersupport.HandlerSupport
import simx.core.svaractor.SVarActor

import scala.reflect.ClassTag

/**
  * Created by dwiebusch on 17.01.16.
  */
trait HandlerIntegrationSupport[ActorType <: SVarActor ] extends HandlerSupport{
  protected val actorContext : ActorType

  private def register[RequiredActorType >: ActorType <: SVarActor , T : ClassTag](h: HandlerRegisterRequest[RequiredActorType, T])
                                                                                  (implicit ct : ClassTag[RequiredActorType]){
    if (ct.runtimeClass isAssignableFrom actorContext.getClass)
      addHandler[T](h.handler.callFunction(_)(actorContext))
  }

  addHandler[HandlerRegisterRequest[_ >: ActorType <: SVarActor, _]]{
    register(_)
  }
}

object Extend {
  case class Into protected[Extend] (ref : SVarActor.Ref){
    def by[Ctxt <: SVarActor, P : ClassTag, R](f : RemoteFunction1[Ctxt, P, R]) = {
      ref ! HandlerRegisterRequest(f)
      ref
    }
  }

  def actor(ref : SVarActor.Ref) = Into(ref)
}

private case class HandlerRegisterRequest[RequiredActorType <: SVarActor, MSGType : ClassTag](handler : RemoteFunction1[RequiredActorType, MSGType, _])

class TestActor extends SVarActor with HandlerIntegrationSupport[SVarActor]

object HandlerIntegrationTest{
  def main(args: Array[String]) {
    val testActor = SVarActor.createActor(new TestActor)

    // integrate function
    Extend actor testActor by new RemoteFunction1[TestActor, String, Unit]{
      def apply(v1: String) = {
        println(v1, actorContext)
        actorContext.context.system.shutdown()
      }
    }

    // test it
    testActor ! "Hallo Welt"
  }
}