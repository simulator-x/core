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

import simx.core.entity.Entity
import simx.core.svaractor.SVarActor
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

/**
  * Created by dwiebusch on 17.01.16.
  */
trait LateContextBinding[ActorContext <: SVarActor]{
  private var _context : Option[ActorContext] = None

  protected implicit def actorContext : ActorContext =
    _context.getOrElse(throw new Exception("actor context was not bound"))

  final def bind(implicit context : ActorContext) {
    _context = Some(context)
  }
}

sealed trait RemoteFunctionBase[ActorContext <: SVarActor] extends LateContextBinding[ActorContext]{
  protected def receiver : Option[SVarActor.Ref]

  receiver.foreach(_ ! this)
}

abstract class RemoteFunction[ActorContext <: SVarActor, P1, P2,P3,P4, R](val paramCount : Int, protected val receiver : Option[SVarActor.Ref] = None)
  extends RemoteFunctionBase[ActorContext]
{
  def apply(p1 : P1) : R = throw new NotImplementedError()
  def apply(p1 : P1, p2 : P2) : R = throw new NotImplementedError()
  def apply(p1 : P1, p2 : P2, p3 : P3) : R = throw new NotImplementedError()
  def apply(p1 : P1, p2 : P2, p3 : P3, p4 : P4) : R = throw new NotImplementedError()

  def callFunction(p1 : P1)(implicit ctxt : ActorContext) = {
    assert(paramCount == 1)
    bind(ctxt)
    apply(p1)
  }

  def callFunction(p1 : P1, p2 : P2)(implicit ctxt : ActorContext) = {
    assert(paramCount == 2)
    bind(ctxt)
    apply(p1, p2)
  }

  def callFunction(p1 : P1, p2 : P2, p3 : P3)(implicit ctxt : ActorContext) = {
    assert(paramCount == 3)
    bind(ctxt)
    apply(p1, p2, p3)
  }

  def callFunction(p1 : P1, p2 : P2, p3 : P3, p4 : P4)(implicit ctxt : ActorContext) = {
    assert(paramCount == 4)
    bind(ctxt)
    apply(p1, p2, p3, p4)
  }
}

abstract class MySpecial2ParamEffect(remote : SVarActor.Ref)
  extends RemoteFunction[EntityUpdateHandling, Entity, Entity, Entity, Entity, Unit](2, Some(remote))
{
  override def apply(p1: Entity, p2: Entity):Unit   = effect(p1, p2)

  // either set this or use DSL from below
  protected var effect: (Entity, Entity) => Unit =
    (_, _) => throw new NotImplementedError()

  // DSL
  protected def effectFor(e : (Entity, Entity) => Unit) =
    effect = e

  object For {
    def parameters(e : (Entity, Entity) => Unit) = effectFor(e)
  }
}

abstract class RemoteFunction1[ActorContext <: SVarActor, P1, R](protected val receiver : Option[SVarActor.Ref] = None)
  extends (P1 => R) with RemoteFunctionBase[ActorContext]
{
  def callFunction(p1 : P1)(implicit ctxt : ActorContext) = {
    bind(ctxt)
    apply(p1)
  }
}

abstract class RemoteFunction2[ActorContext <: SVarActor, P1, P2, R](protected val receiver : Option[SVarActor.Ref] = None)
  extends ((P1, P2) => R) with RemoteFunctionBase[ActorContext]
{
  def callFunction(p1 : P1, p2: P2)(implicit ctxt : ActorContext) = {
    bind(ctxt)
    apply(p1, p2)
  }
}

// Example class
abstract class Effect(remote : SVarActor.Ref)
  extends RemoteFunction2[EntityUpdateHandling, Entity, Entity, Unit](Some(remote))
{
  // rename apply function to effect
  final def apply(v1: Entity, v2: Entity): Unit = effect(v1, v2)

  // either set this or use DSL from below
  protected var effect: (Entity, Entity) => Unit =
    (_, _) => throw new NotImplementedError()

  // DSL
  protected def effectFor(e : (Entity, Entity) => Unit) =
    effect = e

  object For {
    def parameters(e : (Entity, Entity) => Unit) = effectFor(e)
  }
}

// ------------ //
// Test Section //
// ------------ //
class ExecutingActor extends SVarActor with EntityUpdateHandling{
  override protected def removeFromLocalRep(e: Entity): Unit = {}

  addHandler[MySpecial2ParamEffect]{
    msg =>
      println("received " + msg)
      msg.callFunction(new Entity, new Entity)
  }
}



class RequestingActor(anotherActor : SVarActor.Ref) extends SVarActor{
  protected def someInternalActorFunction()(implicit executingActor : EntityUpdateHandling) = {
    println("sender   " + actorContext.self.path)
    println("executor " + executingActor.self.path)
    SVarActor.shutdownSystem()
  }

  new MySpecial2ParamEffect(anotherActor){
    effectFor { (entity1, entity2) =>
      someInternalActorFunction()
      println("\tentity1: " + entity1 +"\n\tentity2: " + entity2)
    }
  }
}


object TestRemoteFunction{
  def main(args: Array[String]) {
    SVarActor.createActor(new RequestingActor(SVarActor.createActor(new ExecutingActor)))
  }
}

