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

package simx.core.ontology.referencesystems

import java.util.UUID

/**
 * User: dwiebusch
 * Date: 16.05.11
 * Time: 11:26
 */

trait ReferenceSystem[T]{
  private var mappings = Map[UUID, T]()
  private var inverses = Map[UUID, T]()

  protected def getMapping( id : UUID ) =
    mappings.get( id )

  protected def getInverse( id : UUID ) =
    inverses.get( id )

  protected def addMapping( id : UUID, value : T, inverse : T ) : UUID = {
    mappings = mappings.updated(id, value)
    inverses = inverses.updated(id, inverse)
    id
  }

  protected def convertTo[U, V]( toConvert : U, inSystem : T, outSystem : T ) : V
}

case class NoMappingException(name : String) extends Exception("Could not find mapping for " + name)
case class NoConversionPossibleException(obj : Any)  extends Exception("Can not convert " + obj.toString)