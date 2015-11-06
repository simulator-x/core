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

package simx.core.svaractor.semantictrait.example

/**
 *
 *
 * Created by dennis on 27.09.15.
 */

import simplex3d.math.floatx.Vec3f
import simx.core.entity.Entity
import simx.core.entity.description.SValSet
import simx.core.ontology.types._
import simx.core.svaractor.SVarActor
import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.semantictrait.base._
import simx.core.svaractor.semantictrait.example.traits.Vehicle
import simx.core.svaractor.semantictrait.example.relations.{affectedBy, has}
import simx.core.svaractor.semantictrait.example.types.{SemanticEntity, Shape, Anything, Location, SteeringBehavior}
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling
import scala.language.reflectiveCalls
import scala.util.continuations
import Iterate.wrapIt

/**
 * Created by dwiebusch on 27.11.14
 */

object Test2{
  def test(v : Radius): Unit ={
//    println(v)
  }

  val r = Radius(1)
  val a = Angle(1)
  test(r)

  def main(args: Array[java.lang.String]) {
//    SetLocationAction -> SetRadiusAction
    SVarActor.createActor(new SVarActor with EntityUpdateHandling {


      override protected def removeFromLocalRep(e: Entity){}

      /**
       * called when the actor is started
       */
      override protected def startUp() = continuations.reset {
        simx.core.ontology.types.init()

        val e = SemanticEntity(new Entity())
        val wheels = for (i <- 1 to 4) yield SemanticEntity(new Entity())

        Iterate over wheels foreach { wheel =>
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
          Container(SValSet()) apply

        if ( e isA Vehicle ) {
          //println((Vehicle(carEntity) attain has(Radius(2))).get.get(Radius))
//          val vehicle = carEntity as Vehicle
//          vehicle moveTo Location("Wuerzburg")
        }

//        val k = (carEntity as Vehicle)



//        def as[V <: SpecificSemanticTrait[_ <: Thing]](x : V):x.SpecificEntityType ={
//          null.asInstanceOf[x.SpecificEntityType]
//        }

      }
    })
  }
}


case class X(in : Semantic.Entity[_ <: Thing]){
  def isA(a : SpecificSemanticTrait[_ <: Thing])(implicit context : EntityUpdateHandling) : scala.Boolean@CPSRet =
    a.tryApply(in).isDefined


//  def as[V <: SpecificSemanticTrait[_ <: Thing]](x : V)(implicit context : EntityUpdateHandling) : x.SpecificEntityType@CPSRet ={
//    x.apply(in).asInstanceOf[x.SpecificEntityType]
//  }
}


object Iterate{
  implicit def wrapIt[L <: Thing](in : Semantic.Entity[L]) : X = X(in)

  def over[T](i : Iterable[T]) = Iterate(i)
}

case class Iterate[T](i : Iterable[T]) {
  def foreach[U](handler: T => U@CPSRet): Unit@CPSRet = if (i.nonEmpty) {
    handler(i.head)
    Iterate(i.tail).foreach(handler)
  }
}


object Test3{
  def test(v : Radius): Unit ={
    //    println(v)
  }

  val r = Radius(1)
  val a = Angle(1)
  test(r)

  import simplex3d.math.float.{Vec3, Mat4}

  def main(args: Array[java.lang.String]) {
    simx.core.ontology.types.init()
    //    SetLocationAction -> SetRadiusAction
    SVarActor.createActor(new SVarActor with EntityUpdateHandling {


      override protected def removeFromLocalRep(e: Entity){}

      /**
       * called when the actor is started
       */
      override protected def startUp() = continuations.reset {
        val e1 = SemanticEntity(new Entity())
        val walls = for (i <- 1 to 4) yield SemanticEntity(new Entity())

        Iterate over walls foreach { wall =>
          wall modify has(Material("Concrete")) set Position apply()
          wall set Scale(Mat4.Identity)
          e1 set has(wall)
        }

        val house  = e1.modify(Position(Vec3.Zero)).
          set(Scale(Mat4.Identity)).apply

        If (e1 isA House)
          House(house) highlight types.Location("Door")
      }
    })
  }


}






object ConcreteWall extends SemanticTrait(Position :: Scale, has(Material("Concrete")))
object House extends SpecificSemanticTrait( Position :: Scale, has(ConcreteWall) ){
  final type SpecificEntityType = SemanticEntity{ def highlight(s : Location.ValueType) }

  protected def createEntity(e: SemanticEntity.ValueType)(implicit creator : SVarActor) = new SemanticEntity(e) {
    def highlight(s : Location.ValueType) =
      println(s + " of entity " + entity + " is now highlighted")
  }
}



