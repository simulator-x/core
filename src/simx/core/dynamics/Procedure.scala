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

package simx.core.dynamics

import simx.core.svaractor.{SimXMessage, SVarActor}
import simx.core.ontology.GroundedSymbol
import scala.annotation.meta.param


/**
 * Created by IntelliJ IDEA.
 * User: dwiebusch
 * Date: 24.02.12
 * Time: 10:42
 */

case class WakeUp()(implicit @(transient @param) actor : SVarActor.Ref) extends SimXMessage

abstract class Procedure( override val name : GroundedSymbol, var frequency : Double )
  extends SVarActor with Method
{
  addHandler[WakeUp]{ msg => if (isStopped) loop() else execute(loop _) }
  private var isStopped = false

  def end(){
    isStopped = true
  }

  override def isDone =
    isStopped

  override protected def startUp(){
    begin()
    loop()
  }


  override def shutdown() {
    end()
    super.shutdown()
  }

  protected def begin(){
    isStopped = false
  }

  protected def loop(){
    if (!isStopped)
      this.addJobIn( (1000.0/frequency).toLong) { self ! WakeUp }
  }
}