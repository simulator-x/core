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

import simx.core.svaractor.handlersupport.If
import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.semantictrait.base.Semantic.Value
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling



/**
 * Created by dwiebusch on 30.11.14
 */

abstract class RelationDescription[T, U, S <: GroundedSymbolFeatures](tpe1 : Class[T], tpe2 : Class[U], val s : S)
  extends RelationDescriptionBase(tpe1,tpe2, BaseValueDescription(s))

sealed abstract class RelationDescriptionBase[T, U, S <: Thing](tpe1 : Class[T], tpe2 : Class[U], desc : ValueDescription[Base, S])
  extends SemanticType[(Semantic.Value[T], Semantic.Value[U]), Base, S](
    classOf[(Semantic.Value[T], Semantic.Value[U])], desc
  )
{
  final type ValueTuple = (Semantic.Value[T], Semantic.Value[U])

  override def apply(v: (Semantic.Value[T], Semantic.Value[U])) : SemanticValue[ValueTuple,  S ]   =
    new Relation[Semantic.Value[T], Semantic.Value[U], S ](valueDescription, v)

  def apply(v : Semantic.Value[U]) =
    new PartialRelation(this, v)

  def apply(number: Int => Boolean, v : Semantic.Value[U]) =
    new PartialRelation(this, v, number)

}

object PartialRelation extends BasicGroundedSymbol{
  val valueDesc = BaseValueDescription(this)
}

class PartialRelation[T, U, S <: Thing](val desc : RelationDescriptionBase[T, U, S], val obj : Semantic.Value[U], number : Int => Boolean = _ => false)
  extends SemanticValue[PartialRelation[T, U, S], PartialRelation.SymbolType]
{
  def apply(subj : Semantic.Value[T])  = desc.apply(subj -> obj)

  // pattern matching would cause cps warning...
  override def subsumes(that: Value[_])(implicit context: EntityUpdateHandling) : Boolean@CPSRet =
    new If (that.isInstanceOf[Relation[_, Semantic.Value[_]@unchecked, _]]) Then {
      val r = that.asInstanceOf[Relation[_, Semantic.Value[_], _]]
      r.valueDescription.groundedSymbol == desc.valueDescription.groundedSymbol && obj.subsumes(r.value._2)
    } ElseIf that.isInstanceOf[ Value[T]@unchecked] Then {
      ABox.matches(that, apply(that.asInstanceOf[Value[T]]))
    }  Else
      false


  override def equals(obj: Any): Boolean = obj match {
    case that : PartialRelation[_, _, _] => desc == that.desc && that.obj == obj
    case _ => false
  }

  override def toString: String = "PartialRelation " + desc + " " + obj
  override val valueDescription = PartialRelation.valueDesc
  override val value: PartialRelation[T, U, S] = this
}


