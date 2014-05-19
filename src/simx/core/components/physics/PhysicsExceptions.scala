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

package simx.core.components.physics

/*
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 5/5/11
 * Time: 3:42 PM
 */
import java.lang.Throwable

object Level extends Enumeration {
  val info = Value("info")
  val warn = Value("warn")
  val error = Value("error")
}

/**
 *  Holds information to JBulletComponent exceptions
 */
case class PhysicsException(errorMessage: String, level: Enumeration#Value = Level.error) extends java.lang.Throwable {
  override def toString: String =  {
    val callerClass = (new Throwable).getStackTrace.apply(1).getClass.getSimpleName
    "[" + level.toString + "]" + "[" + callerClass + "]" + errorMessage
  }
}