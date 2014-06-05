/*
 * Copyright 2012 The SIRIS Project
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

package simx.core.entity.typeconversion

import scala.reflect.ClassTag

/* author: dwiebusch
 * date: 27.08.2010
 */

/**
 *  base class for unified handling of Provides and Requires
 */
abstract class ProvideAndRequire[T, O]
//O : OntologyType
//T : ComponentType

/**
 *  a class that wraps a ProvideConversionInfo. O should always be the type used in the Simulator Core
 */
case class Provide[T, O]( wrapped : ProvideConversionInfo[T, O] ) extends ProvideAndRequire[T, O]{
  //! loop the using method through
  def using( conv : IConverter[T, O] ) =
    Provide[T, O](wrapped.using(conv))

  /**
   *  sets the initial value of the svar to be created
   * @param value the initial value
   * @return the same Provide having the initial value variable set 
   */
  def withInitialValue( value : T ) : Provide[T,O] =
    Provide(wrapped.setInitialValue( value ))

  def asConst =
    Provide(wrapped.asConst)
}

/**
 *  a class that wraps a RequireConversionInfo. O should always be the type used in the Simulator Core
 */
case class Require[O, T]( wrapped : RequireConversionInfo[O, T] ) extends ProvideAndRequire[T, O]{
  //! loop the using method through
  def using( conv : IReverter[T, O] ) =
    Require[O, T](wrapped.using(conv))
}

case class Own[O : ClassTag]( wrapped : ConvertibleTrait[O] )
  extends ProvideAndRequire[O, O]