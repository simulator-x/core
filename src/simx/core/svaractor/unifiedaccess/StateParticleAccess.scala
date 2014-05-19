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

import simx.core.svaractor.{MultiObserve, SVarActor, StateParticle}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.description.SVal
import simx.core.ontology

/**
 * Created by dennis on 24.04.14.
 *
 */
trait StateParticleAccess extends UnifiedAccess{
  type SelfType >: this.type
  protected def access[T](c : ConvertibleTrait[T], actorContext : EntityUpdateHandling ) : (AnnotatedMap[T] => Any) => Unit
  protected def handleNewValue[T](c : SVal[T], handler : SelfType => Any)(implicit actor : EntityUpdateHandling)

  def onUpdate(handler : StateParticleAccess => Any)(implicit actorContext : EntityUpdateHandling) : java.util.UUID
  def ignore(id : java.util.UUID)(implicit actorContext : EntityUpdateHandling)
  def getAllStateParticles(implicit context : EntityUpdateHandling) : Iterable[(Symbol, Set[ontology.Annotation], StateParticle[_])]

  final def observe(c1 : ConvertibleTrait[_], c2 : ConvertibleTrait[_], cs : ConvertibleTrait[_]*)(implicit context : MultiObserve) = {
    context.observe(this, (c1 :: c2 :: cs.toList).toSeq :_*)
  }

  final def observe[T](c : ConvertibleTrait[T], ignoredWriters : Set[SVarActor.Ref] = Set()) =
    new ObservableAccessSet[T, T](this, access(c, _),  _.observe(ignoredWriters), x => x)

  final def get[T](c : ConvertibleTrait[T]) =
    new AccessSet[T, T](access(c, _), _.accessValue, x => x )

  final def set[T](c : SVal[T], handler : SelfType => Any = (_ : SelfType) => {})
                  (implicit actorContext : EntityUpdateHandling)
  {
    access(c.typedSemantics.asConvertibleTrait, actorContext ){ toSet =>
      toSet.filter(_._1 equals AnnotationSet(c.typedSemantics.annotations.toSeq :_*)) match{
        case set if set.size < 2 =>
          implicit val ct = c.typedSemantics.classTag
          handleNewValue(c, handler)
        case set =>
          throw new Exception("Ambiguous selection of StateParticles in 'set': " + set + " when asking for " + c.typedSemantics)
      }
    }
  }
}
