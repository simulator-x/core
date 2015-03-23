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

import akka.pattern.ask
import akka.util.Timeout
import org.scalatest.Matchers
import simx.core.svaractor.SVarActor

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

/**
 *
 * Created by dennis on 26.09.14.
 */
trait ActorTest extends Matchers{
  def createActor[ T <: SVarActor : ClassTag ](ctor : => T, name : Option[String] ) : SVarActor.Ref =
    SVarActor.createActor(ctor, name)

  def createActor[ T <: SVarActor : ClassTag ](ctor : => T, name : String ) : SVarActor.Ref =
    SVarActor.createActor(ctor, Some(name))

  def runTest[T <: ActorTestMsg](test : T, testedActor : SVarActor.Ref, maxTestDuration : Timeout = Timeout.longToTimeout(30000)) {
    val future = testedActor.?(test)(maxTestDuration)
    val result = Await.result(future, Duration.Inf).asInstanceOf[Boolean]
    assert(result)
  }
}

abstract class ActorTestMsg