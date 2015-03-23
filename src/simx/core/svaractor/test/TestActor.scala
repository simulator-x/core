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

import akka.actor.ActorRef
import simx.core.svaractor.SVarActor

import scala.reflect.ClassTag

/**
 *
 * Created by dennis on 26.09.14.
 */
trait TestActor extends SVarActor{
  protected def registerTest[T <: ActorTestMsg : ClassTag]( test : T => ((ActorRef, Boolean) => Unit) => Unit) {
    addHandler[T]{
      msg => test(msg)( _ ! _ )
    }
  }

  protected def provideResult(testResult : Boolean) : ((ActorRef, Boolean) => Unit) => Unit = {
    val replyTo = sender()
    (func: (ActorRef, Boolean) => Unit) => func(replyTo, testResult)
  }

  protected def provideResult(fillInResult : (Boolean => Unit) => Unit) : ((ActorRef, Boolean) => Unit) => Unit = {
    val replyTo = sender()
    (f: (ActorRef, Boolean) => Unit) => fillInResult(result => f(replyTo, result))
  }

  protected def askForResult(receiver : SVarActor.Ref, question : Any) =
    provideResult(ask[Boolean](receiver, question)(_))

  protected def waitFor(mSecs : Long, maxTries : Int = -1)(condition : => Boolean)(eval : => Boolean)  =
    _waitFor(mSecs, maxTries)(condition)(eval)(_)

  private def _waitFor(time : Long, maxTries : Int = -1)(condition : => Boolean)(eval : => Boolean)(exec : Boolean => Unit){
    addJobIn(time){
      if (condition)
        exec(eval)
      else if (maxTries != 0)
        waitFor(time, maxTries -1)(condition)(eval)(exec)
    }
  }

}
