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

package simx.anypackage

import simx.core.entity.Entity
import simx.core.ontology.{EntityDescription, types, Symbols}
import simx.core.svaractor.unifiedaccess._
import simx.core.worldinterface.WorldInterfaceHandling
import simx.core.svaractor.SVarActor
import akka.actor.{ActorRef, Props}
import simx.core.entity.component.EntityCreationHandling
import simx.core.ontology.types.Transformation
import simplex3d.math.floatx.Mat4f
import scala.util.Random
import simx.core.worldinterface.naming.NameIt
import scala.Some


/**
 * Created by dwiebusch on 04.03.14
 */





class Son()(implicit actor : EntityUpdateHandling) extends Entity(){
  val myid = Random.nextInt()
  override def toString: String = "Son" + myid
}

class Father()(implicit actor : EntityUpdateHandling) extends Son {

}


case object StartMSG

case class CreateEntity()

class TestActor2 extends SVarActor with WorldInterfaceHandling with EntityCreationHandling{
  addHandler[CreateEntity]{
    msg =>
      delayedReplyWith[StateParticleAccess](new Entity().set(Transformation(Mat4f(-1f)), _))()
  }

  protected def removeFromLocalRep(e : Entity){

  }

}

class TestActor(that : ActorRef) extends SVarActor with WorldInterfaceHandling with EntityCreationHandling{
  //  override protected implicit val actorContext: this.type = this
  protected def removeFromLocalRep(e : Entity){}

  addHandler[StartMSG.type ]{ msg =>
    println("here we go")

    new EntityDescription(NameIt("test")).realize{
      e =>
        //        e.observe((e : Entity) => println(e))
//        println("realized " + e)
//        var k = 0f
//        e.observe(types.Transformation withAnnotations(Symbols.left)).foreach {
//          (a, v) => println(a , v)
//        }
        //      x.observe{
        //        newEntity : Entity =>
        //          x = newEntity
        //          x.observe(Transformation).foreach{
        //            (a, x) => println(a, "really changed: " + x)
        //          }
        //          println("Entity Changed to " + newEntity)
        //          k = k+1
        //          newEntity.set(types.Velocity(Vec3f.Zero))
        //          newEntity.set(Transformation.withAnnotations(Symbols.left, Symbols.bottom)(Mat4f(k+1)))
        //          newEntity.remove(Transformation)
        //          newEntity.get(Transformation.withAnnotations(Symbols.left, Symbols.top)) foreach {
        //            (a, v) => println("get works, too " + a + " " + v)
        //          }
        //      }


//        println("setting transformation")
//        e.set(Transformation.withAnnotations(Symbols.left, Symbols.top).apply(Mat4f(2)))
//        println("done setting transformation")
//        e.set(Transformation.withAnnotations(Symbols.left, Symbols.bottom).apply(Mat4f(k)))
//
////        e.remove()
//        e.get(Transformation.withAnnotations(Symbols.left, Symbols.top)) foreach {
//          (a, y) => println(a, y)
//        }
//
//        e.observe(Transformation.withAnnotations(Symbols.left, Symbols.top)) foreach {
//          (a, y) => println(a, y)
//        }

        case class GrandFather() extends Father(){
          override def toString: String = "GrandFather"
        }

        case class FatherOf() extends RelationDescription[Entity, Entity](types.Entity, Symbols.parentElement, types.Entity)
        val fatherOf = FatherOf()

        //      relations
        val son = new Son()
        val son2 = new Son()
        val dad = GrandFather()

//        dad.observe{
//          e : Entity => println(dad + ": " + e)
//        }

        dad.observe(fatherOf -> ?){
          (_, s) => println("dad.observe(fatherOf -> ?)", s)
        }

      ?.get(fatherOf -> dad)
      ?.observe(fatherOf -> son).foreach{
        (_, e) => println("!!" + e)
      }

//        son.observe{
//          e : Entity => println(son + ": " + e)
//        }

//        dad.observe((e : Entity) => println(e) )


        dad.set(fatherOf -> son)
        dad.set(fatherOf -> son2)
        dad.set(fatherOf -> son)

      //        dad.observe(fatherOf -> ? ).foreach{
      //          (_, e)  => println("fatherOf.get(dad -> ?)", e)
      //        }


      //        ?.get(fatherOf -> son).foreach{
//          (x, f) => println("?.get(fatherOf -> son)", f)
//        }
//


        fatherOf.get(? -> son).foreach{
          (_, e) => println("fatherOf.get(? -> son)", e)
        }

        fatherOf.get(son -> ?).foreach{
          (_, e) => println("fatherOf.get(? -> son)", e)
        }
//


//        fatherOf.remove(dad -> son)
//        dad.set(fatherOf -> son)
    }
  }
}

object Test{
  def main(args : Array[String]){
    val thatActor = SVarActor.createActor(Props.create(classOf[TestActor2]), Some("TestActor2"))
    SVarActor.createActor(Props.create(classOf[TestActor], thatActor), Some("TestActor")) ! StartMSG
  }
}

