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

import java.lang.reflect.Constructor
import java.net.InetAddress

import akka.actor.Props
import simx.core.component.{Component, ExecutionStrategyHandling}
import simx.core.components.io.IORegistryHandling
import simx.core.entity.Entity
import simx.core.entity.component.{ComponentEntityDescription, _}
import simx.core.helper.{ExecSerialized, Execute, IO, Splash}
import simx.core.ontology.{EntityDescription, types}
import simx.core.svaractor.SVarActor
import simx.core.svaractor.SVarActor.Ref
import simx.core.svaractor.handlersupport.Types.CPSRet
import simx.core.svaractor.unifiedaccess.{EntityBase, EntityUpdateHandling}
import simx.core.worldinterface.WorldInterfaceHandling

import scala.collection.immutable
import scala.reflect.{ClassTag, classTag}

/**
 *
 * Helper class to create objects starting the main actor of a Simulator X application
 *
 * @tparam T the type of the application to be started
 */
abstract class SimXApplicationMain[T <: SimXApplication : ClassTag] private ( p : Array[String] => Props ){
  def this(constructor : => T) =
    this(_ => Props.apply(constructor))

  def this() = this(args => {
    val runtimeClass = classTag[T].runtimeClass
    if (runtimeClass.getConstructors.toSet[Constructor[_]].map(_.getParameterTypes).exists(c =>
      c.contains(args.getClass) && c.length == 1)
    )
      Props(runtimeClass, args)
    else
      Props(runtimeClass)
  })

  def main(args : Array[String]) {
    if (args.contains("enableRemoting"))
      SVarActor.setHostName(InetAddress.getLocalHost.getHostAddress)
    SVarActor.setProfiling(  args.contains( "--profiling" ) )
    SVarActor.setSystemName(classTag[T].runtimeClass.getSimpleName)
    SVarActor.createActor(p(args), None)
  }

  @deprecated("Use simx.core.helper.IO.askForOption instead.", "17-06-2015")
  def askForOption(question: String) = IO.askForOption(question)
}

/**
 * The base trait for all Simulator X applications
 *
 * @author Dennis Wiebusch, Martin Fischbach
 */
trait SimXApplication extends SVarActor with SimXConfig with WorldInterfaceHandling with EntityCreationHandling
with ExecutionStrategyHandling with IORegistryHandling with EntityUpdateHandling
{
  simx.core.ontology.types.init()

  /** the Simulator X splash screen */
  protected val splash = Splash()
  private var componentCreators = List[(SVarActor.Ref => Unit) => Unit]()  
  /** enabling or disabling debug output */
  protected var debug = true

  /**
   * Shows the splash screen and starts the component creation.
   */
  override def startUp() {
    super.startUp()
    splash.show()
    onStartUp()
    create(applicationConfiguration)
    if(componentCreators.nonEmpty) Execute.allInParallel(componentCreators.reverse).exec((_) => {componentsCreated()})
  }

  /**
   * Executed directly at the start of this actor
   */
  protected def onStartUp() {}

  /**
   * Creates a specific component
  
   * @param creator method creating the respective component
   * @tparam T type of the respective component
   */
  private def addCreation[T <: Component : ClassTag](
    creator : (SVarActor.Ref => Unit) => Unit,
    info : Option[String] = None
  ) = {
    val handler = new ComponentHandler
    def createComponent(next: SVarActor.Ref => Unit) {
      if (debug)
        println("[info][SimXApplication] Creating " + info.getOrElse(classTag[T].runtimeClass.getSimpleName))
      creator(handler and next)
    }
    componentCreators = (x => createComponent(x)) :: componentCreators
    handler
  }

  protected def create(cfg : ApplicationConfig) = addCreation( { handler =>
    cfg.realize{ appEntity =>
      appEntity.get(types.Entity).forall {
        entities =>
          val tasks = entities.values.map {
            e => (x : (SVarActor.Ref, EntityBase[_]) => Unit) => e.get{ y => y.get(types.Component).foreach{ x(_, y) } }
          }
          serialize(tasks.toList).apply(handler)
      }
    }

//  }, "\n\t" + cfg.componentAspects.map(_.runtimeClass.getSimpleName).mkString(" and\n\t") )

  }, Some("\n\t" + cfg.componentAspects.map(_.runtimeClass.getSimpleName).mkString(" and\n\t")) )



  private def serialize(x : List[((SVarActor.Ref, EntityBase[_]) => Unit) => Unit]): (SVarActor.Ref => Unit) => Unit = x match{
    case Nil => handler => handler(self)
    case head :: tail => handler => head{ (actor, entity) => serialize(tail)(handler) }
  }

  protected class ComponentHandler(private var handleComp : SVarActor.Ref => Any = _ => ()) extends Serializable{
    private[SimXApplication] def and(next : SVarActor.Ref => Unit)(comp : SVarActor.Ref) {
      handleComp(comp)
      next(comp)
    }
  }

  private var additionalConfiguration = List[() => Unit]()
  private var configurationDone = false

  /**
   * Add a function that will be executed after the createEntities step and before the finishConfiguration step.
   * This method should be called during object construction
   */
  protected def addAdditionalConfigurationStep(configure: () => Unit): Unit = {
    if(!configurationDone)
      additionalConfiguration ::= configure
    else
      println("[warn][SimXApplication] " +
        "'addAdditionalConfigurationStep' called during or after configuration application. " +
        "Discarding passed function. " +
        "The method 'addAdditionalConfigurationStep' should be called during object construction.")
  }

  /**
   * Should be called when all components are created. The configuration of components and creation of entities will be
   * started and the method finishConfiguration() be called.
   * @note This method is called automatically if you used the create method to create the components
   */
  protected def componentsCreated() {
    println("[info][SimXApplication] Components created")
    handleRegisteredComponents(components => {
      println("[info][SimXApplication] Configuring components")
      configureComponents(components)
      println("[info][SimXApplication] Creating entities")
      createEntities()
      splash.dispose()
      configurationDone = true
      additionalConfiguration.foreach(_.apply())
      println("[info][SimXApplication] Configuring application")
      finishConfiguration()
    })
  }

  addHandler[ScheduleShutdownIn]{ msg =>
    this.addJobIn(msg.millis){
      super.shutdown()
      SVarActor.shutdownSystem()
    }
  }

  private case class ScheduleShutdownIn(millis: Long)

  /**
   * Shuts down the application
   */
  override def shutdown() {
    self ! ScheduleShutdownIn(500L)
  }

  /**
   * Defines the components that [[simx.core.SimXApplication]] has to create
   */
  protected def applicationConfiguration: ApplicationConfig

  /**
   * Called after all components were created
   * @param components the map of components, accessible by their names
   */
  protected def configureComponents(components: immutable.Map[Symbol, SVarActor.Ref])

  /**
   * Called when the entities are meant to be created
   */
  protected def createEntities()

  /**
   * Called after components were configured and the creation of entities was initiated
   */
  protected def finishConfiguration()

  /**
   * Used to initialize the application (e.g. load libraries on startup)
   */
  protected def initialize() {}

  /**
   * Realizes a "loading screen entity",
   * then realizes a list of other entities,
   * then removes the "loading screen entity" again.
   */
  protected def realizeWithLoadingScreen(
    loadingScreenDesc: EntityDescription,
    toRealize: List[(EntityDescription, Entity => Unit)]
  ) {
    var loadingScreenEntity: Option[Entity] = None
    var nextHandler: Option[Entity => Unit] = None
    var execDesc: ExecSerialized[_,_] =
      ExecSerialized[Entity, Entity](loadingScreenDesc.realize, () => null).
        andThen(prevResults => {
          loadingScreenEntity = Some(prevResults._2)
          nextHandler = Some(toRealize.head._2)
          toRealize.head._1.realize})

    toRealize.tail.foreach(desc => execDesc = execDesc.andThen(prevResults => {
      nextHandler.collect{case handler => handler.apply(prevResults.asInstanceOf[(Any, Entity)]._2)}
      nextHandler = Some(desc._2)
      desc._1.realize
    }))
    execDesc.exec(prevResults => {
      nextHandler.collect{case handler => handler.apply(prevResults.asInstanceOf[(Any, Entity)]._2)}
      loadingScreenEntity.collect{case ent => ent.remove()}
    })
  }

  this.initialize()
}

object ApplicationConfig{
  def withComponent[T <: Component](componentAspect : ComponentAspect[T]) =
    ApplicationConfig(componentAspect :: Nil)
}

case class ApplicationConfig(componentAspects: List[ComponentAspect[_ <: Component]])
  extends EntityDescription(componentAspects.map( ComponentEntityDescription(_) ).toList, 'ApplicationConfig)
{
  def and[T <: Component](componentAspect : ComponentAspect[T]) =
    ApplicationConfig(componentAspects :+ componentAspect)

  def andIf(condition : Boolean, componentAspect : ComponentAspect[_ <: Component]) =
    if(condition) and(componentAspect) else this

  def iff(condition: Boolean) =
    if(condition) this else ApplicationConfig(componentAspects.init)

  def on(name : String) =
    onNode(name)

  def onNode(name : String) =
    ApplicationConfig(componentAspects.init :+ componentAspects.last.onNode(name))
}


trait Continuations extends SimXApplication{
  override protected def componentsCreated() {
    println("[info][SimXApplication] Components created")
    handleRegisteredComponents(configureComponents)
  }


  /**
   * Called after all components were created
   * @param components the map of components, accessible by their names
   */
  override final protected def configureComponents(components: Map[Symbol, Ref]) =
    util.continuations.reset{
      println("[info][SimXApplication] Configuring components")
      configureComponentsCPS(components)
      println("[info][SimXApplication] Creating entities")
      createEntitiesCPS()
      splash.dispose()
      println("[info][SimXApplication] Configuring application")
      finishConfigurationCPS()
    }

  /**
   * Called when the entities are meant to be created
   */
  override final protected def createEntities(){
    println("[warn][SimXApplication] plain createEntities was called in a CPS configuration")
    util.continuations.reset{
      createEntitiesCPS()
      splash.dispose()
      println("[info][SimXApplication] Configuring application")
      finishConfiguration()
    }
  }

  /**
   * Called after components were configured and the creation of entities was initiated
   */
  override final protected def finishConfiguration() =
    util.continuations.reset( finishConfigurationCPS() )

  /**
   * Called when the entities are meant to be created
   */
  protected def createEntitiesCPS() : Unit@CPSRet

  /**
   * Called after all components were created
   * @param components the map of components, accessible by their names
   */
  protected def configureComponentsCPS(components: Map[Symbol, Ref]): Unit@CPSRet

  /**
   * Called after components were configured and the creation of entities was initiated
   */
  protected def finishConfigurationCPS(): Unit@CPSRet
}