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

package simx.core.svaractor.semantictrait.example

import simx.core.svaractor.semantictrait.base.{ExtendedGroundedSymbol, BasicGroundedSymbol}

/**
 *
 * Created by dennis on 23.01.15.
 */
package object symbols {
  object any extends BasicGroundedSymbol
  object at extends BasicGroundedSymbol

  object container extends BasicGroundedSymbol

  object location extends BasicGroundedSymbol

  object gravity extends BasicGroundedSymbol
  object position extends BasicGroundedSymbol
  object scale extends BasicGroundedSymbol
  object steering extends BasicGroundedSymbol
  object shape extends BasicGroundedSymbol
  object string extends BasicGroundedSymbol

  object number extends BasicGroundedSymbol

  object integer extends number.Extension

  object affectedBy extends BasicGroundedSymbol

  object in extends BasicGroundedSymbol

  object has extends BasicGroundedSymbol


}
