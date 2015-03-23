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

import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.language.reflectiveCalls

/**
 *
 * Created by dennis on 11.11.14.
 */
object CPST {
  implicit def cpsIterable[A, Repr](xs: IterableLike[A, Repr]) = new {
    def cps = new {
      def foreach[B](f: A => Any@CPSRet): Unit@CPSRet = {
        val it = xs.iterator
        while (it.hasNext) f(it.next())
      }

      def map[B, That](f: A => B@CPSRet)(implicit cbf: CanBuildFrom[Repr, B, That]): That@CPSRet = {
        val b = cbf(xs.repr)
        foreach(b += f(_))
        b.result()
      }

      def find(f : A=>Boolean@CPSRet)(implicit cbf: CanBuildFrom[Repr, A, Repr]) : Option[A]@CPSRet= {
        var retVal : Option[A] = None
        val it = xs.iterator
        while(retVal.isEmpty && it.hasNext) {
          val elem = it.next()
          if (f(elem))
            retVal = Some(elem)
        }
        retVal
      }

      def forall(f: A => Boolean@CPSRet)(implicit cbf: CanBuildFrom[Repr, A, Repr]): Boolean@CPSRet = {
        var retVal = true
        val it = xs.iterator
        while (retVal && it.hasNext)
          retVal &&= f(it.next())
        retVal
      }

      def filter(f: A => Boolean@CPSRet)(implicit cbf: CanBuildFrom[Repr, A, Repr]): Repr@CPSRet = {
        val b = cbf(xs.repr)
        for (x <- this)
          if (f(x)) b += x
        b.result()
      }

      def foldLeft[B](z: B)(f: (B, A) => B@CPSRet): B@CPSRet = {
        val it = xs.iterator
        var acc: B = z
        while (it.hasNext) acc = f(acc, it.next())
        acc
      }

      def reduceLeft[B >: A](f: (B, A) => B@CPSRet): B@CPSRet = {
        if (xs.isEmpty)
          throw new UnsupportedOperationException("empty.reduceLeft")

        val it = xs.iterator
        var acc: B = it.next()
        while (it.hasNext) acc = f(acc, it.next())
        acc
      }
    }
  }
}

object ABox{
  import CPST.cpsIterable

  private var map = Map[Semantic.Value[_], Set[Semantic.Value[_]]]()

  def set[T, T1 <: Thing, T2 <: Thing](value    : SemanticValue[T, T1], relation : SemanticValue[_, T2])
                                      (handler : SemanticValue[T, T1 with T2] => Unit)
                                      (implicit context: EntityUpdateHandling) = synchronized {
    map = map.updated(value, map.getOrElse(value, Set()).
      filter(x => x.isInstanceOf[Relation[_, _, _]] || !x.valueDescription.subsumes(relation.valueDescription)) + relation)
    handler(value.asInstanceOf[SemanticValue[T, T1 with T2]])
  }

  def get[R](value : Semantic.Value[_], relation : SemanticValue[_, _<: Thing])(handler : Iterable[R] => Unit)(implicit context: EntityUpdateHandling){
    util.continuations.reset{
      val x = directAccess(value, relation).map(_.asInstanceOf[R])
      handler(x)
    }
  }

  protected def directAccess(value : Semantic.Value[_], relation : SemanticValue[_, _<: Thing])(implicit context: EntityUpdateHandling) =
    map.getOrElse(value, Set()).cps.filter(cand => relation.subsumes(cand))

  def matches(a : Semantic.Value[_], b : Semantic.Value[_])(implicit context: EntityUpdateHandling) =
    directAccess(a, b).nonEmpty

  override def toString: String =
    "ABox:\n\t" + map.mkString("\n\t")
}
