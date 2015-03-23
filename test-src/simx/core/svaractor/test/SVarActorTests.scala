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

package simx.core.svaractor.test

import java.util.UUID

import akka.util.Timeout
import org.scalatest.{FunSpec, Matchers}
import simx.core.entity.Entity
import simx.core.ontology.types
import simx.core.svaractor.SVarActor
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

/**
 *
 * Created by dennis on 26.09.14.
 */


case class CreateEntity() extends ActorTestMsg
case class CreateSVar(value : String) extends ActorTestMsg
case class SVarEquals(value : String) extends ActorTestMsg
case class SVarEqualsMulti(value : String, times : Int) extends ActorTestMsg
case class ForeignSVarEqualsMulti(value : String, times : Int) extends ActorTestMsg

class TestActorInst extends TestActor with EntityUpdateHandling{


  override protected def removeFromLocalRep(e: Entity) {}

  private val knownEntities = collection.mutable.Map[UUID, Entity]()
  private case class DelayResult(value : Entity)

  registerTest[CreateEntity]{ msg  =>
    val retVal = new Entity()
    knownEntities += retVal.id -> retVal
    provideResult(retVal != null)
  }

  registerTest[CreateSVar]{ msg =>
    provideResult{ fillInResult =>
      knownEntities.head._2.set(
        types.String(msg.value),
        (e : Entity)=> fillInResult(e.sVars.contains(types.String.sVarIdentifier))
      )
    }
  }

  registerTest[SVarEquals]{ msg =>
    provideResult(fillResult => knownEntities.head._2.get(types.String).head{
      str => fillResult(str equals msg.value)
    })
  }

  registerTest[SVarEqualsMulti]{ msg =>
    var equalCount = 0
    var neqCount = 0
    for (i <- 0 until msg.times) {
      knownEntities.head._2.get(types.String).head {
        str =>
          if (str equals msg.value) equalCount += 1
          else neqCount += 1
      }
    }
    provideResult(waitFor(mSecs = 100){neqCount + equalCount == msg.times}{
      equalCount == msg.times
    })
  }

  registerTest[ForeignSVarEqualsMulti]{ msg =>
    class ForeignActor(e : Entity) extends SVarActor with EntityUpdateHandling{
      override protected def removeFromLocalRep(e: Entity){}

      addHandler[SVarEquals]{
        msg => delayedReply[String](e.get(types.String).head)
      }
    }

    var neqCount = 0
    var equalCount = 0
    val sendGroupSize = 1000
    assert(msg.times % sendGroupSize == 0)
    val foreigner = spawnActor(new ForeignActor(knownEntities.head._2))
    for (i <- 1 to msg.times / sendGroupSize) {
      addJobIn(i * sendGroupSize / 5) {
        println("sending request group nr. " + i + " (containing " + sendGroupSize + " requests)")
        for (j <- 0 until sendGroupSize) {
          ask[String](foreigner, SVarEquals(msg.value)) {
            str =>
              if (str equals msg.value) equalCount += 1
              else neqCount += 1
          }
        }
      }
    }
    provideResult(waitFor(mSecs = 500){
      println("got " + (neqCount + equalCount) + " of " + msg.times+ " replies")
      neqCount + equalCount == msg.times
    }{ equalCount == msg.times })
  }


  addHandler[DelayResult]{ msg =>
    val pAnswer = provideAnswer
    addJobIn(1000) {
      pAnswer( msg.value != null )
    }
    DelayedAnswer
  }
}


class SVarActorTests extends FunSpec with ActorTest{

  private implicit val maxTestDuration = Timeout.longToTimeout(30000)
  val actor = SVarActor.createActor(new TestActorInst, Some("MainTestActor"))
  val initialSVarValue = "firstValue"

  describe("An entity") {
    it("should be createable") {
      runTest(CreateEntity(), actor)
    }

    describe("regarding set") {
      it("an actor should be able to create svars") {
        runTest(CreateSVar(initialSVarValue), actor)
      }

      it("the owner should be able to read value of svars"){
        runTest(SVarEquals(initialSVarValue), actor)
      }

      it("the owner should be able to read value of svars multipleTimes"){
        runTest(SVarEqualsMulti(initialSVarValue, 1000), actor)
      }

      it("another actor should be able to read value of svars multipleTimes"){
        runTest(ForeignSVarEqualsMulti(initialSVarValue, 100000), actor, maxTestDuration = Timeout.longToTimeout(300000))
      }
    }
  }
}
