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

package simx.core.svaractor.test

import actors.AbstractActor
import simx.core.svaractor._

case class StartTest( sender : AbstractActor )
case class TestFinished( sender : AbstractActor, successful : Boolean )
case class OwnerChangeRetry[T]( sender: AbstractActor, count: Int, sVar: SVar[T], a: AbstractActor )
case class CheckValue[T]( sender: AbstractActor, sVar: SVar[T] )
case class ValueChangeRetry[T]( sender: AbstractActor, count: Int, sVar: SVar[T] )
case class IgnoreWait( sender: AbstractActor, sVarIgnoreWaitCount: Int )

/*class SVarSpec extends Spec with ShouldMatchers {

  class SVarActorImplementation extends SVarActor
  type SVarDataType = String
  val sVarObject : SVarObjectInterface = SVarImpl
  val sVarInitValue : SVarDataType = "Hallo"
  val sVarWriteValue : SVarDataType = "Stephan"

  val ownerChangeRetryLimit: Int = 10000
  val valueChangeRetryLimit: Int = 10000
  val sVarIgnoreWaitCount: Int = 10000

  describe("An SVar") {

    describe("should be createable") {

      it( "local in a SVarActor" ) {

        class TestActor extends SVarActorImplementation {
          addHandler[StartTest]
                    {
                      case StartTest( sender ) =>
                        val sVar = sVarObject( sVarInitValue )
                        sender ! TestFinished( Actor.self, sVar != null )
                    }
        }
        val sVarActor = new TestActor
        sVarActor.start
        sVarActor ! StartTest( Actor.self )
        Actor.self.receive(
          {
            case TestFinished( sender, successful ) =>
              sVarActor.shutdown
              Thread.sleep(100)
              assert( successful )
          }
          )
      }


      it( "remote on a given SVarActor" ) {
        val sVarActor = new SVarActorImplementation
        sVarActor.start
        val sVar = sVarObject( sVarInitValue, sVarActor )
        sVarActor.shutdown
        Thread.sleep(100)
        assert( sVar != null )
      }

    }

    describe("should have the correct owner when created") {
      it( "local" ) {
        class TestActor extends SVarActorImplementation {
          addHandler[StartTest]
                    {
                      case StartTest( sender ) =>
                        val sVar = sVarObject( sVarInitValue )
                        sender ! TestFinished( Actor.self, sVar.owner == Actor.self )
                    }
        }
        val sVarActor = new TestActor
        sVarActor.start
        sVarActor ! StartTest( Actor.self )
        Actor.self.receive( {
          case TestFinished( sender, successful ) =>
            sVarActor.shutdown
            Thread.sleep(100)
            assert( successful )
        })
      }

      it( "remotely" ) {
        var actor : Option[SVarActor] = None
        class TestActor extends SVarActorImplementation {
          addHandler[StartTest](
            {
              case StartTest( sender ) =>
                actor = Some(new SVarActorImplementation)
                actor.get.start
                val sVar = SVarImpl.applyC( sVarInitValue, actor.get )
                SVarImpl.applyC( sVarInitValue, actor.get )
                SVarImpl.applyC( sVarInitValue, actor.get )
                if( sVar != null ) {
                  sender ! TestFinished( Actor.self, sVar.owner == actor.get )
                } else {
                  sender ! TestFinished( Actor.self, false )
                }
            }
            )
        }
        val sVarActor = new TestActor
        sVarActor.start
        sVarActor ! StartTest( Actor.self )
        Actor.self.receive( {
          case TestFinished( sender, successful ) =>
            actor.collect{ case actor : SVarActor => actor.shutdown }
            sVarActor.shutdown
            Thread.sleep(100)
            assert( successful )
        })
      }
    }

    describe("should have the correct on creation given value when created") {
      it( "local" ) {

        class TestActor extends SVarActorImplementation {
          addHandler[StartTest] {
            case StartTest( sender ) =>
              val sVar = sVarObject( sVarInitValue )
              sender ! TestFinished( Actor.self, sVar.get == sVarInitValue )
          }
        }

        val sVarActor = new TestActor
        sVarActor.start
        sVarActor ! StartTest( Actor.self )
        Actor.self.receive( {
          case TestFinished( sender, successful ) =>
            sVarActor.shutdown
            Thread.sleep(100)
            assert( successful )
        })
      }

      it( "remotely" ) {
        var a : Option[SVarActor] = None
        class TestActor extends SVarActorImplementation {
          addHandlerC[StartTest] {
            case StartTest( sender ) =>
              a = Some(new SVarActorImplementation)
              a.get.start
              val sVar = sVarObject( sVarInitValue, a.get )
              sender ! TestFinished( Actor.self, sVar.get == sVarInitValue )
          }
        }

        val sVarActor = new TestActor
        sVarActor.start
        sVarActor ! StartTest( Actor.self )
        Actor.self.receive( {
          case TestFinished( sender, successful ) =>
            a.collect{ case actor : SVarActor => actor.shutdown }
            sVarActor.shutdown
            Thread.sleep(100)
            assert( successful )
        })
      }
    }


    describe("should have the correct value after written from") {
      it( "local" ) {
        class TestActor extends SVarActorImplementation {
          addHandlerC[StartTest]{
            case StartTest( sender ) =>
              val sVar = sVarObject( sVarInitValue )
              sVar.set( sVarWriteValue )
              sender ! TestFinished( Actor.self, sVar.get == sVarWriteValue )
          }
        }

        val sVarActor = new TestActor
        sVarActor.start
        sVarActor ! StartTest( Actor.self )
        Actor.self.receive( {
          case TestFinished( sender, successful ) =>
            sVarActor.shutdown
            Thread.sleep(100)
            assert( successful )
        })
      }

      it( "remote" ) {
        var a : Option[SVarActor] = None
        class TestActor extends SVarActorImplementation {
          addHandlerC[StartTest] {
            case StartTest( sender ) =>
              a = Some(new SVarActorImplementation)
              a.get.start
              val sVar = sVarObject( sVarInitValue, a.get )
              sVar.set( sVarWriteValue )
              sender ! TestFinished( Actor.self, sVar.get == sVarWriteValue )
          }
        }

        val sVarActor = new TestActor
        sVarActor.start
        sVarActor ! StartTest( Actor.self )
        Actor.self.receive( {
          case TestFinished( sender, successful ) =>
            a.collect{ case actor : SVarActor => actor.shutdown }
            sVarActor.shutdown
            Thread.sleep(100)
            println("hier")
            assert( successful )
        })
      }
    }

    describe("should be readable") {
      describe("synchronously from") {
        it( "local" ) {
          class TestActor extends SVarActorImplementation {
            addHandlerC[StartTest]{
              case StartTest( sender ) =>
                val sVar = sVarObject( sVarInitValue )
                sVar.set( sVarWriteValue )
                sender ! TestFinished( Actor.self, sVar.get == sVarWriteValue )
            }
          }

          val sVarActor = new TestActor
          sVarActor.start
          sVarActor ! StartTest( Actor.self )
          Actor.self.receive(
            {
              case TestFinished( sender, successful ) =>
                sVarActor.shutdown
                Thread.sleep(100)
                assert( successful )
            }
            )
        }

        it( "remote" ) {
          var a : Option[SVarActor] = None
          class TestActor extends SVarActorImplementation {
            addHandlerC[StartTest]{
              case StartTest( sender ) =>
                a = Some(new SVarActorImplementation)
                a.get.start
                val sVar = sVarObject( sVarInitValue, a.get )
                sVar.set( sVarWriteValue )
                sender ! TestFinished( Actor.self, sVar.get == sVarWriteValue )
            }
          }

          val sVarActor = new TestActor
          sVarActor.start
          sVarActor ! StartTest( Actor.self )
          Actor.self.receive(
            {
              case TestFinished( sender, successful ) =>
                a.collect{ case actor : SVarActor => actor.shutdown }
                sVarActor.shutdown
                Thread.sleep(100)
                assert( successful )
            }
            )
        }
      }

      describe("asynchronously from") {
        it( "local" ) {
          class TestActor extends SVarActorImplementation {
            addHandler[StartTest]{
              case StartTest( sender ) =>
                val sVar = sVarObject( sVarInitValue )
                sVar.set( sVarWriteValue )
                sVar.get(
                  (v: SVarDataType) => {
                    sender ! TestFinished( Actor.self, v == sVarWriteValue )
                  }
                  )

            }
          }

          val sVarActor = new TestActor
          sVarActor.start
          sVarActor ! StartTest( Actor.self )
          Actor.self.receive(
            {
              case TestFinished( sender, successful ) =>
                sVarActor.shutdown
                Thread.sleep(100)
                assert( successful )
            }
            )
        }

        it( "remote" ) {
          var a : Option[SVarActor] = None
          class TestActor extends SVarActorImplementation {
            addHandler[StartTest]{
              case StartTest( sender ) =>
                a = Some(new SVarActorImplementation)
                a.get.start
                val sVar = sVarObject( sVarInitValue, a.get )
                sVar.set( sVarWriteValue )
                sVar.get(
                  (v: SVarDataType) => {
                    sender ! TestFinished( Actor.self, v == sVarWriteValue )
                  }
                  )
            }
          }

          val sVarActor = new TestActor
          sVarActor.start
          sVarActor ! StartTest( Actor.self )
          Actor.self.receive(
            {
              case TestFinished( sender, successful ) =>
                a.collect{ case actor : SVarActor => actor.shutdown }
                sVarActor.shutdown
                Thread.sleep(100)
                assert( successful )
            }
            )
        }

      }
    }

    describe("'s owner can be changed by") {

      describe( "the owner" ) {
        it("itself") {
          var a : Option[SVarActor] = None
          class TestActor extends SVarActorImplementation {
            addHandler[StartTest]{
              case StartTest( sender ) =>
                a = Some(new SVarActorImplementation)
                a.get.start
                val sVar = sVarObject( sVarInitValue )
                sVar.owner( a.get )
                Actor.self ! OwnerChangeRetry( sender, ownerChangeRetryLimit, sVar, a.get)
            }
            addHandler[OwnerChangeRetry[_]]{
              case OwnerChangeRetry( sender, 0, sVar, _ ) =>
                sender ! TestFinished( Actor.self, false )

              case OwnerChangeRetry( sender, count, sVar, b ) =>
                if( sVar.owner == b )
                  sender ! TestFinished( Actor.self, true )
                else
                  Actor.self ! OwnerChangeRetry( sender, count-1, sVar, b )
            }
          }

          val sVarActor = new TestActor
          sVarActor.start
          sVarActor ! StartTest( Actor.self )
          Actor.self.receive(
            {
              case TestFinished( sender, successful ) =>
                a.collect{ case actor : SVarActor => actor.shutdown }
                sVarActor.shutdown
                Thread.sleep(100)
                assert( successful )
            }
            )
        }

        describe("and the value is not changing") {
          it( "when setted before owner change" ) {
            var actor : Option[SVarActor] = None
            class TestActor extends SVarActorImplementation {
              addHandler[StartTest]{
                case StartTest( sender ) =>
                  actor = Some(new SVarActorImplementation)
                  val a = actor.get
                  a.start
                  val sVar = sVarObject( sVarInitValue )
                  sVar.set( sVarWriteValue )
                  sVar.owner( a )
                  Actor.self ! OwnerChangeRetry( sender, ownerChangeRetryLimit, sVar, a )
              }
              addHandler[OwnerChangeRetry[_]]{
                case OwnerChangeRetry( sender, 0, sVar, a ) =>
                  sender ! TestFinished( Actor.self, false )

                case OwnerChangeRetry( sender, count, sVar, a ) =>
                  if( sVar.owner == a )
                    Actor.self ! CheckValue( sender, sVar )
                  else
                    Actor.self ! OwnerChangeRetry( sender, count-1, sVar, a )
              }

              addHandlerC[CheckValue[_]]{
                case CheckValue( sender, sVar ) =>
                  sender ! TestFinished( Actor.self, sVar.get == sVarWriteValue )
              }
            }

            val sVarActor = new TestActor
            sVarActor.start
            sVarActor ! StartTest( Actor.self )
            Actor.self.receive(
              {
                case TestFinished( sender, successful ) =>
                  actor.collect{ case actor : SVarActor => actor.shutdown }
                  sVarActor.shutdown
                  Thread.sleep(100)
                  assert( successful )
              }
              )
          }

          it( "when setted after owner change" ) {
            var actor : Option[SVarActor] = None
            class TestActor extends SVarActorImplementation {
              addHandler[StartTest]{
                case StartTest( sender ) =>
                  actor = Some(new SVarActorImplementation)
                  val a = actor.get
                  a.start
                  val sVar = sVarObject( sVarInitValue )
                  sVar.owner( a )
                  sVar.set( sVarWriteValue )
                  Actor.self ! OwnerChangeRetry( sender, ownerChangeRetryLimit, sVar, a )
              }
              addHandler[OwnerChangeRetry[_]]{
                case OwnerChangeRetry( sender, 0, sVar, a ) =>
                  sender ! TestFinished( Actor.self, false )

                case OwnerChangeRetry( sender, count, sVar, a ) =>
                  if( sVar.owner == a )
                    Actor.self ! CheckValue( sender, sVar )
                  else
                    Actor.self ! OwnerChangeRetry( sender, count-1, sVar, a )
              }

              addHandlerC[CheckValue[_]]{
                case CheckValue( sender, sVar ) =>
                  sender ! TestFinished( Actor.self, sVar.get == sVarWriteValue )
              }
            }

            val sVarActor = new TestActor
            sVarActor.start
            sVarActor ! StartTest( Actor.self )
            Actor.self.receive(
              {
                case TestFinished( sender, successful ) =>
                  actor.collect{ case actor : SVarActor => actor.shutdown }
                  sVarActor.shutdown
                  Thread.sleep(100)
                  assert( successful )
              }
              )
          }
        }


        it("and all observers are still notified") {
          var actor : Option[SVarActor] = None
          class TestActor extends SVarActorImplementation {
            addHandler[StartTest]{
              case StartTest( sender ) =>
                actor = Some(new SVarActorImplementation)
                val a = actor.get
                a.start
                val sVar = sVarObject( sVarInitValue )
                sVar.observe(
                  (v: SVarDataType ) =>  {
                    sender ! TestFinished( Actor.self, v == sVarWriteValue )
                  }
                  )

                sVar.owner( a )
                Actor.self ! OwnerChangeRetry( sender, ownerChangeRetryLimit, sVar, a )
            }
            addHandler[OwnerChangeRetry[_]]{
              case OwnerChangeRetry( sender, 0, sVar, a ) =>
                sender ! TestFinished( Actor.self, false )

              case OwnerChangeRetry( sender, count, sVar : SVar[SVarDataType], a ) =>
                if( sVar.owner == a )
                  sVar.set( sVarWriteValue )

                else
                  Actor.self ! OwnerChangeRetry( sender, count-1, sVar, a )
            }
          }

          val sVarActor = new TestActor
          sVarActor.start
          sVarActor ! StartTest( Actor.self )
          Actor.self.receive(
            {
              case TestFinished( sender, successful ) =>
                actor.collect{ case actor : SVarActor => actor.shutdown }
                sVarActor.shutdown
                Thread.sleep(100)
                assert( successful )
            }
            )
        }
      }

      describe( "the future owner" ) {
        it("") {
          var actor : Option[SVarActor] = None
          class TestActor extends SVarActorImplementation {
            addHandler[StartTest]{
              case StartTest( sender ) =>
                actor = Some(new SVarActorImplementation)
                val a = actor.get
                a.start
                val sVar = sVarObject( sVarInitValue, a )
                sVar.owner( SVarActorImpl.self )
                Actor.self ! OwnerChangeRetry( sender, ownerChangeRetryLimit, sVar, SVarActorImpl.self )
            }
            addHandler[OwnerChangeRetry[_]]{
              case OwnerChangeRetry( sender, 0, sVar, a ) =>
                sender ! TestFinished( Actor.self, false )

              case OwnerChangeRetry( sender, count, sVar, a ) =>
                if( sVar.owner == a )
                  sender ! TestFinished( Actor.self, true )
                else
                  Actor.self ! OwnerChangeRetry( sender, count-1, sVar, a )
            }
          }

          val sVarActor = new TestActor
          sVarActor.start
          sVarActor ! StartTest( Actor.self )
          Actor.self.receive(
            {
              case TestFinished( sender, successful ) =>
                actor.collect{ case actor : SVarActor => actor.shutdown }
                sVarActor.shutdown
                Thread.sleep(100)
                assert( successful )
            }
            )
        }
        describe("and the value is not changing") {
          it( "when setted before owner change" ) {
            var actor : Option[SVarActor] = None
            class TestActor extends SVarActorImplementation {
              addHandler[StartTest]{
                case StartTest( sender ) =>
                  actor = Some(new SVarActorImplementation)
                  val a = actor.get
                  a.start
                  val sVar = sVarObject( sVarInitValue,a )
                  sVar.set( sVarWriteValue )
                  sVar.owner( SVarActorImpl.self )
                  Actor.self ! OwnerChangeRetry( sender, ownerChangeRetryLimit, sVar, SVarActorImpl.self )
              }
              addHandler[OwnerChangeRetry[_]]{
                case OwnerChangeRetry( sender, 0, sVar, a ) =>
                  sender ! TestFinished( Actor.self, false )

                case OwnerChangeRetry( sender, count, sVar, a ) =>
                  if( sVar.owner == a )
                    Actor.self ! CheckValue( sender, sVar )
                  else
                    Actor.self ! OwnerChangeRetry( sender, count-1, sVar, a )
              }

              addHandlerC[CheckValue[_]]{
                case CheckValue( sender, sVar ) =>
                  sender ! TestFinished( Actor.self, sVar.get == sVarWriteValue )
              }
            }

            val sVarActor = new TestActor
            sVarActor.start
            sVarActor ! StartTest( Actor.self )
            Actor.self.receive(
              {
                case TestFinished( sender, successful ) =>
                  actor.collect{ case actor : SVarActor => actor.shutdown }
                  sVarActor.shutdown
                  Thread.sleep(100)
                  assert( successful )
              }
              )
          }

          it( "when setted after owner change" ) {
            var actor : Option[SVarActor] = None
            class TestActor extends SVarActorImplementation {
              addHandler[StartTest]{
                case StartTest( sender ) =>
                  actor = Some(new SVarActorImplementation)
                  val a = actor.get
                  a.start
                  val sVar = sVarObject( sVarInitValue, a )
                  sVar.owner( SVarActorImpl.self )
                  sVar.set( sVarWriteValue )
                  Actor.self ! OwnerChangeRetry( sender, ownerChangeRetryLimit, sVar, SVarActorImpl.self )
              }
              addHandler[OwnerChangeRetry[_]]{
                case OwnerChangeRetry( sender, 0, sVar, a ) =>
                  sender ! TestFinished( Actor.self, false )

                case OwnerChangeRetry( sender, count, sVar, a ) =>
                  if( sVar.owner == a )
                    Actor.self ! ValueChangeRetry( sender, valueChangeRetryLimit, sVar )
                  else
                    Actor.self ! OwnerChangeRetry( sender, count-1, sVar, a )
              }

              addHandlerC[ValueChangeRetry[_]]{
                case ValueChangeRetry( sender, count, sVar ) =>
                  if( sVar.get == sVarWriteValue )
                    sender ! TestFinished( Actor.self, true )
                  else if ( count == 0 )
                    sender ! TestFinished( Actor.self, false )
                  else
                    Actor.self ! ValueChangeRetry( sender, count-1, sVar )
              }
            }

            val sVarActor = new TestActor
            sVarActor.start
            sVarActor ! StartTest( Actor.self )
            Actor.self.receive(
              {
                case TestFinished( sender, successful ) =>
                  actor.collect{ case actor : SVarActor => actor.shutdown }
                  sVarActor.shutdown
                  Thread.sleep(100)
                  assert( successful )
              }
              )
          }
        }
        it("and all observers are still notified") {
          var actor : Option[SVarActor] = None
          class TestActor extends SVarActorImplementation {
            addHandler[StartTest]{
              case StartTest( sender ) =>
                actor = Some(new SVarActorImplementation)
                val a = actor.get
                a.start
                val sVar = sVarObject( sVarInitValue, a )
                sVar.observe(
                  (v: SVarDataType ) =>  {
                    sender ! TestFinished( Actor.self, v == sVarWriteValue )
                  }
                  )

                sVar.owner( SVarActorImpl.self )
                Actor.self ! OwnerChangeRetry( sender, ownerChangeRetryLimit, sVar, Actor.self )
            }
            addHandler[OwnerChangeRetry[_]]{
              case OwnerChangeRetry( sender, 0, sVar, a ) =>
                sender ! TestFinished( Actor.self, false )


              case OwnerChangeRetry( sender, count, sVar : SVar[SVarDataType], a ) =>
                if( sVar.owner == SVarActorImpl.self )
                  sVar.set( sVarWriteValue )

                else
                  Actor.self ! OwnerChangeRetry( sender, count-1, sVar, a )
            }

          }

          val sVarActor = new TestActor
          sVarActor.start
          sVarActor ! StartTest( Actor.self )
          Actor.self.receive(
            {
              case TestFinished( sender, successful ) =>
                actor.collect{ case actor : SVarActor => actor.shutdown }
                sVarActor.shutdown
                Thread.sleep(100)
                assert( successful )
            }
            )

        }
      }

      describe( "any other actor" ) {
        it("") {
          var actor1 : Option[SVarActor] = None
          var actor2 : Option[SVarActor] = None
          class TestActor extends SVarActorImplementation {
            addHandler[StartTest]{
              case StartTest( sender ) =>
                actor1 = Some(new SVarActorImplementation)
                val a1 = actor1.get
                a1.start
                actor2 = Some(new SVarActorImplementation)
                val a2 = actor2.get
                a2.start

                val sVar = sVarObject( sVarInitValue, a1 )
                sVar.owner( a2 )
                // do some operation on the svar to cause owner updating
                sVar.get( v => {})
                Actor.self ! OwnerChangeRetry( sender, ownerChangeRetryLimit, sVar, a2 )
            }
            addHandler[OwnerChangeRetry[_]]{
              case OwnerChangeRetry( sender, 0, sVar : SVar[SVarDataType], a ) =>
                sender ! TestFinished( Actor.self, false )
                sVar.set( sVarWriteValue )

              case OwnerChangeRetry( sender, count, sVar, a ) =>
                if( sVar.owner == a )
                  sender ! TestFinished( Actor.self, true )
                else
                  Actor.self ! OwnerChangeRetry( sender, count-1, sVar, a )
            }
          }

          val sVarActor = new TestActor
          sVarActor.start
          sVarActor ! StartTest( Actor.self )
          Actor.self.receive(
            {
              case TestFinished( sender, successful ) =>
                actor1.collect{ case actor : SVarActor => actor.shutdown }
                actor2.collect{ case actor : SVarActor => actor.shutdown }
                sVarActor.shutdown
                Thread.sleep(100)
                assert( successful )
            }
            )
        }
        describe("and the value is not changing") {
          it( "when set before owner change" ) {
            var actor1 : Option[SVarActor] = None
            var actor2 : Option[SVarActor] = None
            class TestActor extends SVarActorImplementation {
              addHandler[StartTest]{
                case StartTest( sender ) =>
                  actor1 = Some(new SVarActorImplementation)
                  val a1 = actor1.get
                  a1.start
                  actor2 = Some(new SVarActorImplementation)
                  val a2 = actor2.get
                  a2.start
                  val sVar = sVarObject( sVarInitValue,a1 )
                  sVar.set( sVarWriteValue )
                  sVar.owner( a2 )
                  // do some operation on the svar to cause owner updating
                  sVar.get( v => {})
                  Actor.self ! OwnerChangeRetry( sender, ownerChangeRetryLimit, sVar, a2 )
              }
              addHandler[OwnerChangeRetry[_]]{
                case OwnerChangeRetry( sender, 0, sVar, a ) =>
                  sender ! TestFinished( Actor.self, false )

                case OwnerChangeRetry( sender, count, sVar, a ) =>
                  if( sVar.owner == a )
                    Actor.self ! CheckValue( sender, sVar )
                  else
                    Actor.self ! OwnerChangeRetry( sender, count-1, sVar, a )
              }

              addHandlerC[CheckValue[_]]
                        {
                          case CheckValue( sender, sVar ) =>
                            sender ! TestFinished( Actor.self, sVar.get == sVarWriteValue )
                        }
            }

            val sVarActor = new TestActor
            sVarActor.start
            sVarActor ! StartTest( Actor.self )
            Actor.self.receive(
              {
                case TestFinished( sender, successful ) =>

                  sVarActor ! Shutdown(Actor.self)
                  actor1.collect{ case actor : SVarActor => actor ! Shutdown(Actor.self) }
                  actor2.collect{ case actor : SVarActor => actor ! Shutdown(Actor.self) }
                  Thread.sleep(100)
                  assert( successful )
              }
              )
          }

          it( "when setted after owner change" ) {
            var actor1 : Option[SVarActor] = None
            var actor2 : Option[SVarActor] = None
            class TestActor extends SVarActorImplementation {
              addHandler[StartTest]{
                case StartTest( sender ) =>
                  actor1 = Some(new SVarActorImplementation)
                  val a1 = actor1.get
                  a1.start
                  actor2 = Some(new SVarActorImplementation)
                  val a2 = actor2.get
                  a2.start
                  val sVar = sVarObject( sVarInitValue, a1 )
                  sVar.owner( a2 )
                  sVar.set( sVarWriteValue )
                  Actor.self ! OwnerChangeRetry( sender, ownerChangeRetryLimit, sVar, a2 )
              }
              addHandler[OwnerChangeRetry[_]]{
                case OwnerChangeRetry( sender, 0, sVar, a ) =>
                  sender ! TestFinished( Actor.self, false )

                case OwnerChangeRetry( sender, count, sVar, a ) =>
                  if( sVar.owner == a )
                    Actor.self ! ValueChangeRetry( sender, valueChangeRetryLimit, sVar )
                  else
                    Actor.self ! OwnerChangeRetry( sender, count-1, sVar, a )
              }
              addHandlerC[ValueChangeRetry[_]]{
                case ValueChangeRetry( sender, count, sVar ) =>
                  if( sVar.get == sVarWriteValue )
                    sender ! TestFinished( Actor.self, true )
                  else if (count == 0)
                    sender ! TestFinished( Actor.self, false )
                  else
                    Actor.self ! ValueChangeRetry( sender, count-1, sVar )
              }              
            }

            val sVarActor = new TestActor
            sVarActor.start
            sVarActor ! StartTest( Actor.self )
            Actor.self.receive(
              {
                case TestFinished( sender, successful ) =>
                  sVarActor.shutdown
                  actor1.collect{ case actor : SVarActor => actor.shutdown }
                  actor2.collect{ case actor : SVarActor => actor.shutdown }
                  Thread.sleep(100)
                  assert( successful )
              }
              )
          }
        }
        it("and all observers are still notified")  {
          var actor1 : Option[SVarActor] = None
          var actor2 : Option[SVarActor] = None
          class TestActor extends SVarActorImplementation {
            addHandler[StartTest]{
              case StartTest( sender ) =>
                actor1 = Some(new SVarActorImplementation)
                val a1 = actor1.get
                a1.start
                actor2 = Some(new SVarActorImplementation)
                val a2 = actor2.get
                a2.start
                val sVar = sVarObject( sVarInitValue, a1 )
                sVar.observe(
                  (v: SVarDataType ) =>  {
                    sender ! TestFinished( Actor.self, v == sVarWriteValue )
                  }
                  )

                sVar.owner( a2 )
                // do some operation on the svar to cause owner updating
                sVar.get( v => {})
                Actor.self ! OwnerChangeRetry( sender, ownerChangeRetryLimit, sVar, a2 )
            }
            addHandler[OwnerChangeRetry[_]]{
              case OwnerChangeRetry( sender, 0, sVar, a ) =>
                sender ! TestFinished( Actor.self, false )

              case OwnerChangeRetry( sender, count, sVar : SVar[SVarDataType], a ) =>
                if( sVar.owner == a )
                  sVar.set( sVarWriteValue )

                else
                  Actor.self ! OwnerChangeRetry( sender, count-1, sVar, a )
            }

          }

          val sVarActor = new TestActor
          sVarActor.start
          sVarActor ! StartTest( Actor.self )
          Actor.self.receive(
            {
              case TestFinished( sender, successful ) =>
                sVarActor.shutdown
                actor1.collect{ case actor : SVarActor => actor.shutdown }
                actor2.collect{ case actor : SVarActor => actor.shutdown }
                Thread.sleep(100)
                assert( successful )
            }
            )
        }
      }
    }


    describe("can be observed by") {
      it( "the owner" )  {
        class TestActor extends SVarActorImplementation {
          addHandler[StartTest]{
            case StartTest( sender ) =>
              val sVar = sVarObject( sVarInitValue )
              sVar.observe(
                (v: SVarDataType) => {
                  sender ! TestFinished( Actor.self, v == sVarWriteValue )
                }
                )
              sVar.set( sVarWriteValue )
          }
        }
        val sVarActor = new TestActor
        sVarActor.start
        sVarActor ! StartTest( Actor.self )
        Actor.self.receive(
          {
            case TestFinished( sender, successful ) =>
              sVarActor.shutdown
              Thread.sleep(100)
              assert( successful )
          }
          )
      }
      it( "any other actor" )  {
        var actor1 : Option[SVarActor] = None
        class TestActor extends SVarActorImplementation {
          addHandler[StartTest]{
            case StartTest( sender ) =>
              actor1 = Some(new SVarActorImplementation)
              val a = actor1.get
              a.start
              val sVar = sVarObject( sVarInitValue, a )
              sVar.observe(
                (v: SVarDataType) => {
                  sender ! TestFinished( Actor.self, v == sVarWriteValue )
                }
                )
              sVar.set( sVarWriteValue )
          }
        }
        val sVarActor = new TestActor
        sVarActor.start
        sVarActor ! StartTest( Actor.self )
        Actor.self.receive(
          {
            case TestFinished( sender, successful ) =>
              actor1.collect{ case actor : SVarActor => actor.shutdown }
              sVarActor.shutdown
              Thread.sleep(100)
              assert( successful )
          }
          )
      }
    }

    describe("that is observed can be ignored again by") {
      it( "the owner" )  {
        class TestActor extends SVarActorImplementation {
          var ignored = false
          var error = false
          addHandler[StartTest]{
            case StartTest( sender ) =>
              val sVar = sVarObject( sVarInitValue )
              sVar.observe(
                (v: SVarDataType) => {
                  if( ignored ) {
                    sender ! TestFinished( Actor.self, false )
                    error = true
                  } else {
                    sVar.ignore
                    ignored = true
                    Actor.self ! IgnoreWait( sender, sVarIgnoreWaitCount )
                  }

                }
                )
              sVar.set( sVarWriteValue )
          }
          addHandler[IgnoreWait]{
            case IgnoreWait( sender, 0 ) =>
              if( !error )
                sender ! TestFinished( Actor.self, true )

            case IgnoreWait( sender, count ) =>
              Actor.self ! IgnoreWait( sender, count-1 )
          }
        }
        val sVarActor = new TestActor
        sVarActor.start
        sVarActor ! StartTest( Actor.self )
        Actor.self.receive(
          {
            case TestFinished( sender, successful ) =>
              sVarActor.shutdown
              Thread.sleep(100)
              assert( successful )
          }
          )
      }
      it( "any other actor" ) {
        var actor1 : Option[SVarActor] = None
        class TestActor extends SVarActorImplementation {
          var ignored = false
          var error = false
          addHandler[StartTest]{
            case StartTest( sender ) =>
              actor1 = Some(new SVarActorImplementation)
              val a = actor1.get
              a.start
              val sVar = sVarObject( sVarInitValue, a )
              sVar.observe(
                (v: SVarDataType) => {
                  if( ignored ) {
                    sender ! TestFinished( Actor.self, false )
                    error = true
                  } else {
                    sVar.ignore
                    ignored = true
                    Actor.self ! IgnoreWait( sender, sVarIgnoreWaitCount )
                  }

                }
                )
              sVar.set( sVarWriteValue )
          }
          addHandler[IgnoreWait]{
            case IgnoreWait( sender, 0 ) =>
              if( !error )
                sender ! TestFinished( Actor.self, true )

            case IgnoreWait( sender, count ) =>
              Actor.self ! IgnoreWait( sender, count-1 )
          }
        }
        val sVarActor = new TestActor
        sVarActor.start
        sVarActor ! StartTest( Actor.self )
        Actor.self.receive(
          {
            case TestFinished( sender, successful ) =>
              actor1.collect{ case actor : SVarActor => actor.shutdown }
              sVarActor.shutdown
              Thread.sleep(100)
              assert( successful )
          }
          )
      }
    }

    describe("can be read multiple times using the"){
      it("get(<handler>) method"){
        val holderActor = SVarActorImpl.actor{ () => {}}
        holderActor.start
        val toRead = SVarImpl(0, holderActor)
        val numberOfReadings = 1000
        var readings = List[Int]()
        val readingActor = SVarActorImpl.actor{
          SVarActorImpl.self.addHandler[String]{
            case "StartReading" =>
              for ( i <-  0 until numberOfReadings)
                toRead.get( value => {
                  readings ::= i
                })
          }
        }
        readingActor.start
        readingActor ! "StartReading"
        Thread.sleep(100)
        toRead.set(2)
        Thread.sleep(numberOfReadings)
        holderActor.shutdown
        readingActor.shutdown
        assert(readings.length == numberOfReadings)
      }

      it("get@cps method"){
        val holderActor = SVarActorImpl.actor{ () => {}}
        holderActor.start
        val toRead = SVarImpl(0, holderActor)
        val numberOfReadings = 1000
        var readings = List[Int]()
        val readingActor = SVarActorImpl.actor{
          SVarActorImpl.self.addHandler[String]{
            case "StartReading" =>
              var i = 0
              while (i < numberOfReadings){
                Actor.self ! ("Read", i)
                i+=1
              }
          }
          SVarActorImpl.self.addHandlerC[(String, Int)]{
            case ("Read", i : Int) =>
              val value = toRead.get
              readings ::= i
          }
        }
        readingActor.start
        readingActor ! "StartReading"
        Thread.sleep(100)
        toRead.set(2)
        Thread.sleep(numberOfReadings)
        holderActor.shutdown
        readingActor.shutdown
        assert(readings.length == numberOfReadings)
      }
    }

  }
}  */
