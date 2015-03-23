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

package simx.core.svaractor.semantictrait.example

import simplex3d.math.floatx.Vec3f
import simx.core.entity.Entity
import simx.core.entity.description.SValSet
import simx.core.ontology.types.Integer
import simx.core.ontology.types._
import simx.core.svaractor.SVarActor
import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.semantictrait.base._
import simx.core.svaractor.semantictrait.example.actions.Move
import simx.core.svaractor.semantictrait.example.relations.{affectedBy, has}
import simx.core.svaractor.semantictrait.example.traits._
import simx.core.svaractor.semantictrait.example.types.{Shape, Location, Anything, Number, SteeringBehavior, SemanticEntity}
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

import scala.language.reflectiveCalls
import scala.util.continuations

/**
 * Created by dwiebusch on 27.11.14
 */

object Test{
  def testMe(x : Semantic.Entity[ Scale.Type ]){
    println("in testMe")
  }

  def testMe2(x : simx.core.ontology.types.Gravity.ValueType): Unit ={
    println("!in testMe2")
  }


  def main(args: Array[java.lang.String]) {
    SVarActor.createActor(new SVarActor with EntityUpdateHandling {


      override protected def removeFromLocalRep(e: Entity){}

      /**
       * called when the actor is started
       */
      override protected def startUp() = continuations.reset {
        println("start")
        val e = SemanticEntity(new Entity())
        simx.core.ontology.types.init()


        val wheels = for (i <- 1 to 4) yield SemanticEntity(new Entity())

        case class CPS[T](i : Iterable[T]) {
          def foreach[U](handler: T => U@CPSRet): Unit@CPSRet = {
            if (i.nonEmpty) {
              handler(i.head)
              CPS(i.tail).foreach(handler)
            }
          }
        }


        CPS(wheels) foreach {
          wheel =>
            wheel modify has(Shape("round")) set Position set Scale apply()
            e set has(wheel)
        }

        val carEntity = e modify
          has(SteeringBehavior) set
          Gravity set
          Scale set
          Anything set
          Position2D set
          affectedBy(Gravity(Vec3f.Zero)) set
          Container(SValSet()) set

        println("x " +  Integer.valueDescription.groundedSymbol)

        val gr = simx.core.ontology.types.Gravity(-9.81f * simplex3d.math.float.Vec3.UnitY)

        val grav = e attain Gravity

        implicit def tesym2sym(in: BasicGroundedSymbol): GroundedSymbolBase[in.SymbolType] =
          in.Symbol


        testMe2(gr)





        //      e as Movable moveTo Destination("London")

        println("!!" + Vehicle.tryApply(carEntity))
        testMe(traits.Vehicle(carEntity))

        If (Vehicle.tryApply(carEntity).isDefined) Then {
          Move(Movable.tryApply(carEntity).get, Location("Honolulu"))


          val myCar = Movable.tryApply(carEntity)
          Movable(myCar.get).moveTo(Location("Honolulu"))
        }

        val m = Movable.tryApply(e)
        If (m.isDefined) Then {
          println(Vehicle.tryApply(m.get.entity))
        }

        has(Number)

        val list = Container(SValSet())
        //    list set has(Number(1))
        list set Gravity(Vec3f.UnitY)

        //    val g = list get Gravity
        //    val x = Gravity get in(list)
        //    val i = list get has
        val gb = list get Gravity(Vec3f.UnitY)
        //    println(x)

        //e2 set
        val green = Color2("green")
        val e2 = Entity2(1)
        val rel = has2(e -> Color2)
        val rel2 = hasValue2(rel -> green)
        println(rel)
        //    println(rel2)
      }
    })

    //    class S1 extends BasicGroundedSymbol
    //
    //    object s2 extends S1
    //
    //    var n = Number(1)
    //    n = Integer(2)
    //
    //    println(Integer.valueDescription.groundedSymbol.toString , symbols.sinteger.Symbol)
    //
    //    val s1 = new S1 {
    //      override def toString: String = getClass.getSimpleName
    //    }
    //
    //    def test1(a: S1#BaseType): Unit = {
    //
    //    }
    //
    //    def continue[T <: Any](v: T): T@cpsParam[Any, Any] =
    //      shift { (x: T => Any) => println(x(v))} //testActor ! (v, x) }
    //
    //    def fixIfElse() =
    //      shift { (x: Unit => Unit) => x(())}
    //
    //    def If(condition : => Boolean)( x : => Any@cpsParam[Any, Any] ) = {
    //      if (condition) x else fixIfElse()
    //    }
    //
    //    case class Test()
    //
    //
    //    val testActor: SVarActor.Ref = SVarActor.createActor(new SVarActor {
    ////      addHandler[(Any, Any => Unit)]{ f => f._2.apply(f._1)}
    //      addHandler[String] { x : String => if (x == "off")  "off" }
    //      addHandler[Test]( {
    //        i : Test  =>
    //          val svar = createSVar(simx.core.ontology.types.Name("klaus"), Now, Unbuffered)
    //          createActor(new SVarActor {
    //            addHandler[Test]{
    //              t =>
    //                println( svar.read)
    //                context.system.shutdown()
    //            }
    //          })(_ ! Test())()
    //
    //          //          If (false) {
    ////            val x: SVar[Int] = null
    ////             //x.read(this)
    //////            continue(2)
    ////
    ////          }
    //
    ////          continue(1)
    //          "continueing"
    //      } )
    //    })
    //
    //    testActor ! Test()
    //
    //    def doOtherStuff() = {
    //      println("other stuff")
    //      continue(1)
    //    }
    //
    //    def doIt() = {
    //      println("it")
    //      val x = doOtherStuff()
    //      println("got " + x)
    //      x.toLong
    //    }
    //
    //    def myReset(ctx: => Any@cpsParam[Any, Any]) =
    //      reset { ctx }
    //
    //    myReset {
    //
    //
    //      val value = doIt()
    //
    //
    //      println("test")
    ////      Thread.sleep(5000)
    ////      testActor ! "off"
    ////      math.cos(value)
    //
    //
    //      //    object TEx extends Throwable{ override def fillInStackTrace(): Throwable = this }
    //      //    def prepareFunc(in : => Unit) : () => Unit = () => in
    //      //    def break(){ throw TEx }
    //      //
    //      //    var id = 0L
    //      //    def nextId = synchronized{
    //      //      id = id+1
    //      //      id
    //      //    }
    //      //
    //      //    def breakable(o : () => Unit ) {
    //      //      try {
    //      //        o.apply()
    //      //      } catch {
    //      //        case TEx =>
    //      //          val id = nextId
    //      //      }
    //      //    }
    //      //
    //      //
    //      //
    //      //    import System.nanoTime
    //      //
    //      //
    //      //
    //      //    val pf =  prepareFunc{
    //      //      println("test")
    //      //      1
    //      //    }
    //      //    val t1 = nanoTime()
    //      //    breakable(pf)
    //      //    val t2 = nanoTime()
    //      //
    //      //    println((t2-t1) / 1000000.0)
    //
    //      //    test1(s1)
    //
    //      //    val s2 =
    //
    //      val Anything1 = BaseValueDescription(s1.Symbol)
    //      val Anything2 = BaseValueDescription(s2.Symbol)
    //
    //      println(has2Sym.Symbol, Anything2.groundedSymbol)
    //
    //
    //
    //    }
  }
}

object has2Sym extends BasicGroundedSymbol

object hasValue2Sym extends BasicGroundedSymbol

object Entity2Sym extends BasicGroundedSymbol

object Gravity2Sym extends BasicGroundedSymbol

object Color2Sym extends BasicGroundedSymbol

object has2 extends RelationDescription(classOf[simx.core.svaractor.semantictrait.example.types.SemanticEntity.DataType], classOf[Any], has2Sym)
object hasValue2 extends RelationDescription(classOf[has.DataType], classOf[Any], hasValue2Sym)
object Entity2 extends SemanticType(classOf[Int], BaseValueDescription(Entity2Sym))
object Gravity2 extends SemanticType(classOf[Float], BaseValueDescription(Gravity2Sym))
object Color2 extends SemanticType(classOf[java.lang.String], BaseValueDescription(Color2Sym))














