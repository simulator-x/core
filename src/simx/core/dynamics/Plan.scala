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

package simx.core.dynamics

import simx.core.entity.Entity
import simx.core.ontology.types
import simplex3d.math.floatx.{Mat4f, Vec3f}

/**
 * Created by IntelliJ IDEA.
 * User: dwiebusch
 * Date: 24.02.12
 * Time: 10:42
 */

object Plan{
  abstract class MethodBase extends Method{
    def name = null

    def supportedBy(entity: Entity) =
      true

    def effects(target: Entity) =
      Set()

    def inputs =
      Set()
  }

  class A extends MethodBase{
    override def effects =
      Set(Effect(null, types.Transformation(Mat4f.Identity)))

    def apply(target: Entity) { println("A") }

    override def inputs =
      Set()
  }

  class B extends MethodBase{
    override def effects =
      Set( Effect(null, types.Acceleration(Vec3f(0,0,0))) )


    def apply(target: Entity) { println("B") }

    override def inputs =
      Set( Prerequisite(null, types.Transformation(Mat4f.Identity)))
  }

  class C extends MethodBase{
    override def effects =
      Set( Effect(null, types.Velocity(Vec3f(0,0,0))) )


    def apply(target: Entity) { println("C") }

    override def inputs =
      Set(
        Prerequisite(null, types.Acceleration(Vec3f(0,0,0))),
        Prerequisite(null, types.Transformation(Mat4f.Identity))
      )
  }



  def main( args : Array[String] ){
    (new A).bind(new Entity())
    (new B).bind(new Entity())
    (new C).bind(new Entity())
    //println(find(Prerequisite(null, types.Velocity(Vec3(0,0,0))), Set[Prerequisite[_]](), Nil))
    generatePlanFor( Set(), Set(Effect(null, types.Velocity(Vec3f(0,0,0))))).collect{
      case p => p.apply(() => println("done"))
    }
  }


  def generatePlanFor(input : Set[Prerequisite[_]], goalState : Set[Effect[_]]) : Option[Plan] = {
    recurse(input, goalState)._1 match {
      case Nil  => None
      case list => Some(new Plan(list))
    }
  }


  private def find[T](toFind : Prerequisite[T], given : Set[Prerequisite[_]],
                      stack : List[Method]) : (List[Method], Set[Effect[_]]) = {
    //find candidates
    val candidates = Method.getSupportedMethods(toFind.target).map(_.bind(toFind.target)).filter{
      m => m.getEffects.contains(toFind.asEffect) && !stack.contains(m)
    }
    //direct result
    candidates.find( _.getInputs.forall(given contains) ).headOption.collect{
      case result => return (result :: Nil, given.map(_.asEffect) ++ result.getEffects)
    }
    //indirect result
    candidates.filterNot( _.getInputs.forall(given contains) ).map {
      candidate => find(candidate.getInputs, given, candidate :: stack) match {
        case (Nil, _ )   => ( List[Method](), Set[Effect[_]]() )
        case (list, set) => ( list :+ candidate, candidate.getEffects ++ set)
      }
    }.filter( _._1.nonEmpty ).toList.sortWith(_._1.length < _._1.length).headOption.getOrElse( (Nil, Set[Effect[_]]()) )
  }

  private def find( toFind : Set[Prerequisite[_]], given : Set[Prerequisite[_]],
                    stack : List[Method]) : (List[Method], Set[Effect[_]]) = {
    toFind.foldLeft((List[Method](), given.map(_.asEffect))){
      (results, prerequisite) =>
        //Nothing to do here
        if (results._2 contains prerequisite.asEffect) results
        // find sth. or return
        else find(prerequisite, results._2.map(_.asPrerequisite), stack) match {
          case (Nil, _)    => return (Nil, Set())
          case (list, set) => (list ::: results._1, set ++ results._2)
        }
    }
  }

  private def recurse(input : Set[Prerequisite[_]], goalState : Set[Effect[_]],
                      path : List[Method] = Nil) : (List[Method], Set[Effect[_]]) ={
    //Done?
    val result = input.map(_.asEffect)
    if (goalState.forall( result contains))
      return (path, result)
    // else find a solution
    find(goalState.map(_.asPrerequisite), input, path)
  }

}

class Plan( val tasks : List[Method] ){
  def apply( andThen : () => Unit = () => {} ){
    tasks.foldRight( andThen ){
      (task, func) => () => task.execute(func)
    }.apply()
  }
}
