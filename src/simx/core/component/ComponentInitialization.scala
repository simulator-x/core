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

package simx.core.component

import simx.core.SimXApplication

/**
 * User: dwiebusch
 * Date: 26.05.11
 * Time: 09:59
 */

trait ComponentInitialization extends SimXApplication {
  /**
   * do initialization of your component here
   */
  protected def initComponent(){}

  /**
   * This is to ensure other initialize functions are called, too
   */
  override protected def initialize() {
    super.initialize()
    initComponent()
  }

}