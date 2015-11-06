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

package simx.core.worldinterface.base

import simx.core.component.remote.RemoteServiceSupport
import simx.core.svaractor.{BunchOfSimXMessagesMessages, SVarActor, SimXMessage}
import simx.core.worldinterface._

/**
 * Created by martin 
 * on 21/04/15.
 */
private[worldinterface] trait WorldInterfaceActorBase extends SVarActor with RemoteServiceSupport {

  private var foreignWorldInterfaceActors = List[SVarActor.Ref]()

  addHandler[WorldInterfaceActorInCluster] {
    msg => {
      foreignWorldInterfaceActors = foreignWorldInterfaceActors ::: msg.worldInterfaceActor :: Nil
    }
  }

  protected def forwardToForeignActors(msg: ForwardableMessage) {
    if (!msg.isForwarded && foreignWorldInterfaceActors.nonEmpty)
      foreignWorldInterfaceActors.foreach(_.tell(msg.forward(), sender()))
  }

  override protected def startUp() {
    if (SVarActor.isRemotingEnabled) {
      publishActor(WorldInterfaceActor.name, self)
      observeRegistrations(WorldInterfaceActor.name) {
        ref => if (ref != self) addForeignWorldInterfaceActor(ref)
      }
    }
  }

  protected[worldinterface] def createMsg(receiver: SVarActor.Ref)
                                         (msg: SVarActor.Ref => ForwardableMessage, l: List[SimXMessage]) = {
    if (SVarActor isLocal receiver) msg(receiver).forward() :: l else l
  }

  private def addForeignWorldInterfaceActor(ref: SVarActor.Ref) {
    ref ! BunchOfSimXMessagesMessages(generateCopyOfRegistry())
    foreignWorldInterfaceActors = ref :: foreignWorldInterfaceActors
  }

  def generateCopyOfRegistry(): List[SimXMessage]

}
