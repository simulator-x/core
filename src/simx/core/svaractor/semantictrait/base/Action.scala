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

package simx.core.svaractor.semantictrait.base

import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.semantictrait.example.traits.Vehicle
import simx.core.svaractor.semantictrait.example.types.SemanticEntity
import simx.core.svaractor.semantictrait.example.{relations, types}
import Semantic.Value
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling
import scala.language.reflectiveCalls

/**
 *
 * Created by dennis on 02.02.15.
 */

object Action{
  import CPST.cpsIterable
  type StdAction = Action[_, _ <: Base ,_ <: Thing]
  private var registry = Map[SemanticTypeTrait[_, _ <: Base, _ <: Thing], Set[StdAction]]()
  def lookup[T](effects : Set[Semantic.Value[_]])(implicit context: EntityUpdateHandling) : Option[StdAction]@CPSRet =
      registry.values.flatten.cps.find(_ matches effects nonEmpty)

  def lookup[T, B <: Base, S <: Thing](effects : Set[Semantic.Value[_]], retType : SemanticTypeTrait[T, B, S])(implicit context: EntityUpdateHandling) : Option[Action[T, B, S]]@CPSRet =
    registry.getOrElse(retType, Set()).cps.find(_ matches effects nonEmpty).asInstanceOf[Option[Action[T, B, S]]]

  protected def register[T, B <: Base, S <: Thing](c : SemanticTypeTrait[T, B, S], a : Action[T, B, S]) = synchronized{
      registry = registry.updated(c, registry.getOrElse(c, Set()) + a)
  }

  final type Param = SemanticValue[_, _ <: ValueDescription[_ <: Base, _ <: Thing]]
}

sealed trait Position{
  def idx : Int
}

case class Left(idx : Int) extends Position
case class Right(idx : Int) extends Position
case class LeftAndRight(lIdx : Int, rIdx : Int) extends Position{
  def idx = throw new Exception
}

trait ActionBase {
  import CPST.cpsIterable

  val parameters : Iterable[Semantic.Value[_]]
  val preconditions : Set[(Position, Value[_])]
  val effects : Set[(Position, Value[_])]

  def matches(desiredEffects : Set[Value[_]])(implicit context: EntityUpdateHandling) : Option[Map[Int, Value[_]]]@CPSRet = {
    val mappings = desiredEffects.cps.map {
      eff =>
        val matchingEffect = effects.cps.find(_._2.subsumes(eff))
        matchingEffect.collect {
          case (idx: LeftAndRight, mEff: RelationDescription[_, _, _]) =>
            idx.lIdx -> eff.asInstanceOf[Relation[Value[_], _, _]].value._1 ::
              idx.rIdx -> eff.asInstanceOf[Relation[_, Value[_], _]].value._2 ::
              Nil
          case (idx: Right, mEff: PartialRelation[_, _, _]) =>
            idx.idx -> eff.asInstanceOf[Relation[_, Value[_], _]].value._2 :: Nil
          case (idx: Left, mEff: PartialRelation[_, _, _]) =>
            idx.idx -> eff.asInstanceOf[Relation[Value[_], _, _]].value._1 :: Nil
          case (idx: Position, value) => idx.idx -> value :: Nil
        }
    }
    if (mappings.forall(_.isDefined)) {
      val potRet = mappings.flatMap(_.get).toMap
      if (parameters.zip(potRet.values).cps.forall(t => t._1.subsumes(t._2)))
        Some(potRet)
      else
        None
    } else
      None
  }
}

  abstract class Action[T, B <: Base, S <: Thing](val retType : SemanticTypeTrait[T, B, S],
                                                  val parameters : Iterable[Semantic.Value[_]],
                                                  val preconditions : Set[(Position, Value[_])] = Set(),
                                                  val effects : Set[(Position, Value[_])] = Set()) extends ActionBase
  {
    def apply(parameters : Array[Value[_]])(implicit context: EntityUpdateHandling) : SemanticValue[T, S]

    Action.register(retType, this)
  }


object SetLocationAction extends Action(
  retType = types.Location,
  List[Semantic.Value[_]](Vehicle, types.Location),
  preconditions = Set(),
  effects = Set(LeftAndRight(0, 1) -> relations.has)
) {
  def apply(parameters: Array[Value[_]])(implicit context: EntityUpdateHandling) = {
    println("Setting location of " + parameters(0) + " to " + parameters(1))
    SemanticEntity(parameters(0)) modify relations.has(parameters(1))
    types.Location(parameters(1))
  }
}
