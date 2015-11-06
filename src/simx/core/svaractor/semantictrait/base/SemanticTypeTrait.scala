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

package simx.core.svaractor.semantictrait.base

import scala.reflect.runtime.universe.typeOf

/**
 * Created by dwiebusch on 30.11.14
 */
object SemanticTypeTrait{
  private val vdType = typeOf[Thing]
  private val registry = collection.mutable.Map[String, Semantic.Type[_]]()
  protected def register(v : Semantic.Type[_]): Unit = synchronized{
    registry += v.getClass.getName.replaceAll("package\\$", "") -> v
  }

  private def getOrLoad(s : String) : Semantic.Type[_] = {
    registry.get(s + "$") match {
      case Some(thing) => thing
      case None =>
        Class.forName(s.replaceAll("""\.([a-zA-Z]+)$""", ".package\\$$1\\$"))
        getOrLoad(s)
    }
  }

  def lookUp(tpe : reflect.runtime.universe.Type) : Iterable[Semantic.Type[_]] = {
    val a = tpe.baseClasses.filter { _.asType.toType <:< vdType }
    // TODO: remove this hack
    val b = a.head.toString.split(" ").filter(_ contains ".SelfBase").
      map(_.replaceAll(".SelfBase.", "")).
      map(_.replaceAll(".SelfBase", "")).
      map(_.replaceAll("simx.core.ontology.Symbols.", "")).
      map(s => s(0).toUpper + s.tail).
      map("simx.core.ontology.types." + _ )
//      a.filterNot { bc => bc.fullName.contains("<refinement>") || bc.asType.toType.erasure == vdType.erasure}.
    b.map(x => getOrLoad(x))
  }
}

abstract class SemanticType[T, +B <: Base, S <: Thing](protected val tpe : Class[T],
                                                       val valueDescription : ValueDescription[B, S],
                                                       protected val doRegister : Boolean = false)
  extends SemanticTypeTrait[T, B, S] with Serializable
{
  final type ValueType = SemanticValue[T, S]
  final type DataType = T
  final type Type = S
}

trait SemanticTypeTrait[T, +B <: Base, +S <: Thing]
  extends SemanticValue[SemanticTypeTrait[T, B, S], S] with Serializable
{
  protected val tpe : Class[T]
  protected val doRegister : Boolean
  val valueDescription : ValueDescription[B, S]

  val value =
    this

  def ::[X <: Thing](that : SemanticTypeTrait[_, _<: Base, X]) =
    valueDescription.groundedSymbol and that.valueDescription.groundedSymbol

  def apply(v: T): SemanticValue[T, S] =
    FinalSemanticValue(valueDescription, v)

  def apply(v : Semantic.Value[_]) : SemanticValue[T, S] =
    if (v.valueDescription.subsumes(valueDescription) && v.value.getClass == tpe)
      apply(v.value.asInstanceOf[T])
    else
      throw new Exception

  override def toString: String =
    valueDescription + "(" + tpe.getSimpleName + ")"

  override def equals(obj: Any): Boolean = obj match {
    case that : Semantic.Type[_] => tpe == that.tpe && valueDescription == that.valueDescription
    case _ => false
  }

  override def hashCode(): Int =
    tpe.hashCode() + 41 * valueDescription.hashCode()

  if (doRegister)
    SemanticTypeTrait register this // injellij sometimes isn't intellijent enough to figure out that this is correct...
}
