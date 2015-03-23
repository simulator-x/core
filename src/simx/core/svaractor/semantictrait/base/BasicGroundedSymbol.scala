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

/**
 *
 * Created by dennis on 23.01.15.
 */
sealed trait Thing extends Serializable {
  override def toString: String =
    getClass.getSimpleName.replaceAll("\\$", "")
}

sealed abstract class GroundedSymbolBase[+Symbol <: Thing](override val toString : String) extends Serializable
{
  val toSymbol = Symbol(toString)

  override def equals(obj: scala.Any): Boolean = obj match {
    case that : GroundedSymbolBase[_] => that.toSymbol == toSymbol
    case _ => false
  }

  override def hashCode(): Int =
    toSymbol.hashCode()

  def and[ThatSym <: Thing](that : GroundedSymbolBase[ThatSym]) =
    new GroundedSymbolBase[Symbol with ThatSym](toString + " + " + that.toString) {}

  def and[ThatSym <: Thing](that : SemanticTypeTrait[_, _<: Base, ThatSym]) : GroundedSymbolBase[Symbol with ThatSym]=
    and[ThatSym](that.valueDescription.groundedSymbol)
}

sealed trait GroundedSymbolFeatures extends Serializable{
  protected val symbolString = getClass.getName.dropRight(1).replaceAll(".*\\..*\\$", "")

  object Symbol extends GroundedSymbolBase[SymbolType](symbolString)

  protected trait SelfBase extends Thing

  type SymbolType <: Thing

  type Extension = ExtendedGroundedSymbol[SymbolType]

  override def toString: String =
    symbolString

  val toSymbol =
    scala.Symbol(toString)

  val value =
    Symbol

  override def hashCode(): Int =
    Symbol.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case that : GroundedSymbolFeatures => that.Symbol equals Symbol
    case _ => false
  }
}

protected abstract class ExtendedGroundedSymbol[That] extends GroundedSymbolFeatures with Serializable{
  final type SymbolType = That with SelfBase
}

trait BasicGroundedSymbol extends GroundedSymbolFeatures with Serializable{
  final type SymbolType = SelfBase
}

@deprecated("should not be required", "today")
case class ManualGroundedSymbol(override protected val symbolString: String) extends GroundedSymbolFeatures with Serializable{
  override type SymbolType = SelfBase
}

