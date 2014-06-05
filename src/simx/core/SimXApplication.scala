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

package simx.core

import collection.immutable

import components.io.IORegistryHandling
import component.{Component, ExecutionStrategyHandling}
import entity.component.EntityCreationHandling
import worldinterface.WorldInterfaceHandling
import reflect.classTag
import helper.{Execute, Splash}
import svaractor.SVarActor
import scala.reflect.ClassTag

/**
 *
 * Helper class to create objects starting the main actor of a Simulator X application
 *
 * @param ctor the constructor of the main actor of a Simulator X application
 * @tparam T the type of the application to be started
 */
abstract class SimXApplicationMain[T <: SimXApplication](ctor : => T){
  def main(args : Array[String]) {
    SVarActor.createActor(ctor)
  }
}

/**
 * The base trait for all Simulator X applications
 *
 * @author Dennis Wiebusch, Martin Fischbach
 */
trait SimXApplication extends SVarActor with WorldInterfaceHandling with EntityCreationHandling
  with ExecutionStrategyHandling with IORegistryHandling with SimXConfig
{
  simx.core.ontology.types.init()

  /** the Simulator X splash screen */
  protected val splash = Splash()
  private var componentCreators = List[(SVarActor.Ref => Unit) => Unit]()
  private var insideCreateComponents = false
  /** enabling or disabling debug output */
  protected var debug = true

  /**
   * shows the spalsh screen and starts the component creation
   */
  override def startUp() {
    splash.show()
    insideCreateComponents = true; createComponents(); insideCreateComponents = false
    if(componentCreators.nonEmpty) Execute.allInParallel(componentCreators.reverse).exec((_) => {componentsCreated()})
  }

  /**
   * Creates a specific component
   *
   * @note If you use this mehtod to create one of your components, do not use the createActor functions of
   *       [[simx.core.svaractor.SVarActor]] to create other components as the componentsCreated method will be
   *       called by automatically
   * @param constructor constructor of the respective component
   * @tparam T type of the resprective component
   */
  protected def create[T <: Component : ClassTag](constructor : => T) = {
    class ComponentHandler(private var handleComp : SVarActor.Ref => Any = _ => ()){
      def attachHandler(f : SVarActor.Ref => Any){ handleComp = f }
      private[SimXApplication] def and(next : SVarActor.Ref => Unit)(comp : SVarActor.Ref) {
        handleComp(comp)
        next(comp)
      }
    }

    if(insideCreateComponents) {
      val handler = new ComponentHandler
      def createComponent(next: SVarActor.Ref => Unit)  {
        if(debug) println("[Creating] " + classTag[T].runtimeClass.getSimpleName)
        createActor(constructor){handler and next}()
      }
      componentCreators = ( x => createComponent(x) ) :: componentCreators
      handler
    }  else throw new Exception("SimXApplication.create[T <: Component] may only be called within createComponents.")
  }

  /**
   * Should be called when all components are created. The configuration of components and creation of entities will be
   * started and the method finishConfiguration() be called.
   * @note This method is called automatically if you used the create method to create the components
   */
  protected def componentsCreated() {
    println("[Components created]")
    handleRegisteredComponents(components => {
      configureComponents(components)
      createEntities()
      splash.dispose()
      finishConfiguration()
    })
  }

  addHandler[ScheduleShutdownIn]{ case msg =>
    this.addJobIn(msg.millis){
      super.shutdown()
      SVarActor.shutdownSystem()
    }
  }

  private case class ScheduleShutdownIn(millis: Long)

  /**
   * shuts down the application
   */
  override def shutdown() {
    self ! ScheduleShutdownIn(500L)
  }

  /**
   * called when the components used in this application are meant to be created
   */
  protected def createComponents()

  /**
   * called after all components were created
   * @param components the map of components, accessible by their names
   */
  protected def configureComponents(components: immutable.Map[Symbol, SVarActor.Ref])

  /**
   * called when the entities are meant to be created
   */
  protected def createEntities()

  /**
   * called after components were configured and the creation of entities was initiated
   */
  protected def finishConfiguration()

  /**
   * used to initialize the application (e.g. load libraries on startup)
   */
  protected def initialize() {}

  this.initialize()
}
