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

package simx.core.svaractor.handlersupport

import simx.core.benchmarking.CoreBenchmarkingConfiguration
import simx.core.svaractor.SimXMessage

/**
 * User: dwiebusch
 * Date: 23.02.13
 * Time: 14:48
 */
trait HandlerBenchmarking extends HandlerSupportImpl{
  override protected def applyHandlers(msg : Any) : Any = {
    val begin   = if( CoreBenchmarkingConfiguration.enableBenchmarking && CoreBenchmarkingConfiguration.enableHandlerBeginTimeLogging ) System.nanoTime() else 0
    val retVal  = super.applyHandlers(msg)
    val end     = if( CoreBenchmarkingConfiguration.enableBenchmarking && CoreBenchmarkingConfiguration.enableHandlerEndTimeLogging ) System.nanoTime() else 0
    if( CoreBenchmarkingConfiguration.enableBenchmarking && (CoreBenchmarkingConfiguration.enableHandlerBeginTimeLogging || CoreBenchmarkingConfiguration.enableHandlerEndTimeLogging )) {
      val messageId = if( msg.isInstanceOf[SimXMessage]) msg.asInstanceOf[SimXMessage].messageUUID else null
      val creation = if( msg.isInstanceOf[SimXMessage]) msg.asInstanceOf[SimXMessage].creationTimestamp else -1l
      val data = (messageId,creation,begin,end)
      if( this.messageProcessingLog.contains(msg.asInstanceOf[AnyRef].getClass) )
        this.messageProcessingLog = this.messageProcessingLog + (msg.asInstanceOf[AnyRef].getClass -> (this.messageProcessingLog(msg.asInstanceOf[AnyRef].getClass) ::: data :: Nil) )
      else
        this.messageProcessingLog = this.messageProcessingLog + (msg.asInstanceOf[AnyRef].getClass -> (data :: Nil) )
    }
     retVal
  }
}
