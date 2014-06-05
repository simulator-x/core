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

import simx.core.ontology.{Symbols, GroundedSymbol}


class ActionDescription[InType, OutType](val name : GroundedSymbol){
  override def equals(obj: scala.Any): Boolean = obj match {
    case a : ActionDescription[InType, OutType] => a.name equals name
    case _ => false
  }
}

object ActionParticle{
  private var registry =
    Map[ActionDescription[_, _], ActionParticle[_, _]]()

  def register[A, B](desc : ActionDescription[A, B], particle : ActionParticle[A, B]) = synchronized {
    registry = registry.updated(desc, particle)
  }

  def lookup[A, B](name : ActionDescription[A, B]) : ActionParticle[A, B] =
    registry.getOrElse(name, throw new Exception("error ;)")).asInstanceOf[ActionParticle[A, B]]

  def lookupAndAttachTo[A, B, C](name : ActionDescription[B, C], that : ActionParticle[A, B])  : ActionParticle[A, C] =
    that succeededBy lookup(name)
}

/**
 * Created by dwiebusch on 29.12.13
 */
class ActionParticle[In, Out] protected(name : Option[GroundedSymbol], run : In => Out) {
  private def this(run : In => Out) =
    this(None, run)

  private def succeededBy[T](next : ActionParticle[Out, T]) : ActionParticle[In, T] =
    new ActionParticle( (p : In) => next.apply(run(p)) )

  def succeededBy[X](next : ActionDescription[Out, X]) =
    ActionParticle.lookupAndAttachTo(next, this)

  final def apply(p : In) =
    run(p)

  if (name.isDefined)
    ActionParticle.register(new ActionDescription(name.get), this)
}

case class ConcreteAction[ParameterTupleType, Outcome](name : GroundedSymbol)(run : ParameterTupleType => Outcome)
  extends ActionParticle[ParameterTupleType, Outcome](Some(name), run)


object Testing{
  def main(args : Array[String]){
    val x = ConcreteAction(Symbols.left){i : Int => i + 1}
    val y = ConcreteAction(Symbols.right){i : Int => i * 2.5 }

    val k = new ActionDescription[Int, Int](Symbols.left)
    val l = new ActionDescription[Int, Int](Symbols.right)


    println(ActionParticle lookup k succeededBy l apply 1)
  }
}
