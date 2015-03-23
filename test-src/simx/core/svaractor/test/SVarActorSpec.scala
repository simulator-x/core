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

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import org.scalatest.{Matchers, FunSpec}
import simx.core.ontology.types
import simx.core.svaractor.TimedRingBuffer.{Now, Unbuffered}
import simx.core.svaractor._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * Created by IntelliJ IDEA.
 * User: stephan_rehfeld
 * Date: 25.08.2010
 * Time: 12:29:01
 * To change this template use File | Settings | File Templates.
 */




class SVarActorSpec extends FunSpec with Matchers {

  implicit val testTimeOut = Timeout.longToTimeout(30000)
  val sVarObject : SVarObjectInterface = SVarImpl
  val sVarInitValue : SVarDataType = "Hallo"
  val sVarWriteValue : SVarDataType = "Stephan"

  class SVarActorImplementation extends SVarActor
  type SVarDataType = String

  val ownerChangeRetryLimit: Int = 10000
  val waitForPongLimit: Int = 10000

  case class OwnerChangeRetry( sender: SVarActor.Ref, count: Int, sVars: List[SVar[SVarDataType]], a: SVarActor.Ref )
  case class WaitForPong( sender: SVarActor.Ref, count: Int )
  case class Ping( sender: SVarActor.Ref, reply: SVarActor.Ref )
  case class Pong( sender: SVarActor.Ref )


  describe( "should be shutdownable" ) {

    describe( "by it self" ) {

      it( "and give away all SVars" ) {
        class TestActor extends SVarActorImplementation{

          case class Create( sender: SVarActor.Ref )
          case class SVarList( sVars: List[SVar[SVarDataType]])
          case class Observe( sender: SVarActor.Ref, sVars: List[SVar[SVarDataType]] )
          case class Observed( sVars: List[SVar[SVarDataType]] )

          var a1: SVarActor.Ref = null
          var a2: SVarActor.Ref = null
          var s: SVarActor.Ref = null

          addHandler[StartTest]{
            case StartTest(  ) =>


              class TestActor1 extends SVarActorImplementation {
                addHandler[Create](
                {
                  case Create( sender ) =>
                    val sVars = for(  x <- List.range(1, 10 ) ) yield sVarObject.apply( types.String(sVarInitValue), Now, Unbuffered )
                    sender ! SVarList( sVars )

                }
                )
              }

              class TestActor2 extends SVarActorImplementation {
                addHandler[Observe](
                {
                  case Observe( sender, sVars ) =>
                    for( sVar <- sVars) sVar.observe(
                      (v: SVarDataType) => {
                      }
                    )
                    sender ! Observed( sVars )

                }
                )
              }

              a1 = spawnActor(new TestActor1)


              a2 = spawnActor(new TestActor2)

              a1 ! Create( self )
              s = sender()
          }

          addHandler[SVarList]{
            case SVarList( sVars ) =>
              a2 ! Observe( self, sVars )
          }

          addHandler[Observed]{
            case Observed( sVars ) =>
              a1 ! Shutdown()
              self ! OwnerChangeRetry( s, ownerChangeRetryLimit, sVars, a2 )
          }

          addHandler[OwnerChangeRetry]{
            case OwnerChangeRetry( sender, 0, sVars, _ ) =>
              println( "Owners not changed" )
              sender ! TestFinished( self, successful = false )


            case OwnerChangeRetry( sender, count, sVars, actor ) =>
              var allOkay = true
              for( sVar <- sVars )
                if(! owner( sVar ).isSameAs(actor) ) {
                  allOkay = false
                }
              if( allOkay ) {
                sender ! TestFinished( self, successful = true )
              } else
                self ! OwnerChangeRetry( sender, count-1, sVars, actor )


          }

        }
        val sVarActor = SVarActor.createActor(new TestActor, Some("testactor1"))
        val future = sVarActor ? StartTest()
        val result = Await.ready(future, Duration.Inf).asInstanceOf[TestFinished]
        assert(result.successful)
      }

      it( "remove all handler and not react any more" ) {
        class TestActor extends SVarActorImplementation {

          case class Create( sender: SVarActor.Ref )
          case class SVarList( sVars: List[SVar[SVarDataType]])
          case class Observe( sender: SVarActor.Ref, sVars: List[SVar[SVarDataType]] )
          case class Observed( sVars: List[SVar[SVarDataType]] )



          var a1: SVarActor.Ref = null
          var a2: SVarActor.Ref = null
          var s: SVarActor.Ref = null

          addHandler[StartTest]
          {
            case StartTest( ) =>


              class TestActor1 extends SVarActorImplementation {
                addHandler[Create](
                {
                  case Create( sender ) =>
                    val sVars = for(  x <- List.range(1, 10 ) ) yield sVarObject( types.String(sVarInitValue), Now, Unbuffered )
                    sender ! SVarList( sVars )
                }
                )

                addHandler[Ping](
                {
                  case Ping( sender, reply ) =>
                    reply ! Pong( sender )
                }
                )
              }

              class TestActor2 extends SVarActorImplementation {
                addHandler[Observe](
                {
                  case Observe( sender, sVars ) =>
                    for( sVar <- sVars) sVar.observe(
                      (v: SVarDataType) => {
                      }
                    )
                    sender ! Observed( sVars )

                }
                )
              }

              a1 = spawnActor(new TestActor1)

              a2 = spawnActor(new TestActor2)

              a1 ! Create( self )
              s = sender()
          }
          addHandler[SVarList]{
            case SVarList( sVars ) =>
              a2 ! Observe( self, sVars )
          }

          addHandler[Observed]{
            case Observed( sVars ) =>
              a1 ! Shutdown()
              self ! OwnerChangeRetry( s, ownerChangeRetryLimit, sVars, a2 )
          }

          addHandler[OwnerChangeRetry]{
            case OwnerChangeRetry( sender, 0, sVars, _ ) =>
              println( "Owners not changed" )
              sender ! TestFinished( self, successful = false )


            case OwnerChangeRetry( sender, count, sVars, actor ) =>
              var allOkay = true
              for( sVar <- sVars )
                if( !owner( sVar ).isSameAs(actor) ) {
                  allOkay = false
                }
              if( allOkay ) {

                a1 ! Ping( sender, self )
                self ! WaitForPong( sender, waitForPongLimit )
              } else
                self ! OwnerChangeRetry( sender, count-1, sVars, actor )
          }

          addHandler[Pong]{
            case Pong( sender ) =>
              sender ! TestFinished( self, successful = false )
          }

          addHandler[WaitForPong]{
            case WaitForPong( sender, 0 ) =>
              sender ! TestFinished( self, successful = true )

            case WaitForPong( sender, count ) =>
              self ! WaitForPong( sender, count-1 )
          }
        }

        val sVarActor = SVarActor.createActor(new TestActor, Some("testactor2"))
        val future = sVarActor ? StartTest( )
        val result = Await.ready(future, Duration.Inf).asInstanceOf[TestFinished]
        assert(result.successful)
      }
    }
    describe( "from outside" ) {
      it( "and give away all SVars" ) {
        class TestActor extends SVarActorImplementation {
          addHandler[StartTest]{
            case StartTest( ) =>
              val a = spawnActor(new SVarActorImplementation)
              val sVars = for(  x <- List.range(1, 10 ) ) yield sVarObject( types.String(sVarInitValue), Now, Unbuffered )
              for( sVar <- sVars) sVar.observe(
                (v: SVarDataType) => {
                }
              )

              a ! Shutdown()
              self ! OwnerChangeRetry( sender(), ownerChangeRetryLimit, sVars, self )
          }
          addHandler[OwnerChangeRetry]{
            case OwnerChangeRetry( sender, 0, sVars, _ ) =>
              println( "Owners not changed" )
              sender ! TestFinished( self, successful = false )


            case OwnerChangeRetry( sender, count, sVars, actor ) =>
              var allOkay = true
              for( sVar <- sVars )
                if( !owner( sVar ).isSameAs(actor) ) {
                  allOkay = false
                  sVar.set( sVarWriteValue )
                }
              if( allOkay ) {
                sender ! TestFinished( self,successful =  true )
              } else
                self ! OwnerChangeRetry( sender, count-1, sVars, actor )


          }
        }
        val sVarActor = SVarActor.createActor(new TestActor, Some("testactor3"))
        val future = sVarActor ? StartTest()
        val result = Await.ready(future, Duration.Inf).asInstanceOf[TestFinished]
        assert(result.successful)
      }

      it( "remove all handler and not react any more" ) {
        class TestActor extends SVarActorImplementation {
          addHandler[StartTest]{
            case StartTest( ) =>
              class TestActor1 extends SVarActorImplementation {
                addHandler[Ping](
                {
                  case Ping( sender, reply ) =>
                    reply ! Pong( sender )
                }
                )
              }

              val a = spawnActor(new TestActor1)

              val sVars = for(  x <- List.range(1, 10 ) ) yield sVarObject( types.String(sVarInitValue), Now, Unbuffered )
              for( sVar <- sVars) sVar.observe(
                (v: SVarDataType) => {
                }
              )

              a ! Shutdown()
              self ! OwnerChangeRetry( sender(), ownerChangeRetryLimit, sVars, self )
          }
          addHandler[OwnerChangeRetry]{
            case OwnerChangeRetry( sender, 0, sVars, _ ) =>
              println( "Owners not changed" )
              sender ! TestFinished( self, successful = false )


            case OwnerChangeRetry( sender, count, sVars, actor ) =>
              var allOkay = true
              for( sVar <- sVars )
                if( !owner( sVar ).isSameAs(actor) ) {
                  allOkay = false
                  sVar.set( sVarWriteValue )
                }
              if( allOkay ) {
                sender ! TestFinished( self, successful = true )
                actor ! Ping( sender, self )
                self ! ( sender, waitForPongLimit )
              } else
                self ! OwnerChangeRetry( sender, count-1, sVars, actor )
          }
          addHandler[Pong]{
            case Pong( sender ) =>
              sender ! TestFinished( self, successful = false )
          }
          addHandler[WaitForPong]{
            case WaitForPong( sender, 0 ) =>
              sender ! TestFinished( self, successful = true )

            case WaitForPong( sender, count ) =>
              self ! WaitForPong( sender, count-1 )
          }
        }

        val sVarActor = SVarActor.createActor(new TestActor, Some("testactor4"))
        val future = sVarActor ? StartTest()
        val result = Await.ready(future, Duration.Inf).asInstanceOf[TestFinished]
        assert(result.successful)
      }
    }
  }
}
