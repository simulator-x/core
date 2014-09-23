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
import simx.core.ontology.entities.{Arm, User}
import simx.core.ontology.{SVarDescription, EntityDescription, types, Symbols}
import simx.core.svaractor.unifiedaccess._
import simx.core.worldinterface.WorldInterfaceHandling
import simx.core.svaractor.SVarActor
import akka.actor.{ActorRef, Props}
import simx.core.entity.component.EntityCreationHandling
import simx.core.ontology.types.Transformation
import simplex3d.math.floatx.Mat4f
import scala.util.Random
import simx.core.worldinterface.naming.NameIt


/**
 * Created by dwiebusch on 04.03.14
 */





object Son extends SVarDescription(types.Entity as Symbols.saturation){
  val myid = Random.nextInt()
  override def toString: String = "Son" + myid
}

class Father extends  SVarDescription(types.Entity as Symbols.factor) {

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

        object GrandFather extends Father{
          override def toString: String = "GrandFather"
        }

        object HasArm extends RelationDescription(types.Entity, Symbols.parentElement, types.Entity, "")


        //      relations
        val arm = new Arm(new Entity, this)

        val arm2 = new Entity
        val user = new User(new Entity, this)

//        dad.observe{
//          e : Entity => println(dad + ": " + e)
//        }



        val onChangeId = user.observe(HasArm -> ?).onChange{
          case Add(_, value)    => println("ADD:    user.observe(HasArm -> ?).onChange", value)
          case Update(_, value) => println("UPDATE: user.observe(HasArm -> ?).onChange", value)
          case Remove(_, value) => println("REMOVE: user.observe(HasArm -> ?).onChange", value)
        }

//        dad.ignore(onChangeId)
//
        HasArm.observe(user -> arm) head {
          relation => println("HasArm.observe(user -> arm)", relation)
        }
//
        user.get(HasArm -> ?)

        user.observe(HasArm -> ?)(
          entity => println("user.observe(hasArm -> ?)", entity)
        )
//
        user.observe(HasArm -> arm){
          relation => println("user.observe(HasArm -> arm)", relation)
        }
//
      ?.get(HasArm.restrictTypes(types.User, types.Arm) -> arm).foreach{
        x => println("?.get(HasArm.restrictTypes(types.User, types.Arm) -> arm)", x)
      }
      ?.observe(HasArm -> arm).foreach{
        (_, e) => println("?.observe(HasArm -> arm)", e)
      }

//        son.observe{
//          e : Entity => println(son + ": " + e)
//        }

//        dad.observe((e : Entity) => println(e) )


        user.set(HasArm -> arm)
        user.set(HasArm -> arm2)
        user.set(HasArm -> arm)

      //        dad.observe(fatherOf -> ? ).foreach{
      //          (_, e)  => println("fatherOf.get(dad -> ?)", e)
      //        }


      //        ?.get(fatherOf -> son).foreach{
//          (x, f) => println("?.get(fatherOf -> son)", f)
//        }
//


        HasArm.observe(? -> arm).foreach{
          e => println("hasArm.observe(? -> arm)", e)
        }


        HasArm.get(user -> ?).foreach{
          (_, e) => println("hasArm.get(user -> ?)", e)
        }
//
//        fatherOf.observe(? -> son)

        HasArm.remove(user -> arm)
        user.set(HasArm -> arm)
    }
  }
}

object Test{
  def main(args : Array[String]){
    val thatActor = SVarActor.createActor(Props.create(classOf[TestActor2]), Some("TestActor2"))
    SVarActor.createActor(Props.create(classOf[TestActor], thatActor), Some("TestActor")) ! StartMSG
  }
}

