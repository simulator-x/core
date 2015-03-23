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

package simx.core.entity

import description._
import simx.core.ontology._
import simx.core.svaractor.TimedRingBuffer.{Now, ContentType, Time, BufferMode}
import simx.core.svaractor._
import simx.core.svaractor.semantictrait.base.BasicGroundedSymbol
import simx.core.ontology.types.NullType
import simx.core.svaractor.unifiedaccess._
import simx.core.worldinterface.WorldInterfaceHandling
import typeconversion.{IReverter, ConvertibleTrait}
import typeconversion.TypeInfo.DataTag


/**
 * Created by dwiebusch on 27.08.2010
 * updated 10.03.14
 */
private object internalEntityOption extends BasicGroundedSymbol
private object SelfDesc extends SValDescription(NullType as internalEntityOption withType classOf[Option[Entity]])

class Entity private(val description : GeneralEntityDescription,
                     val id : java.util.UUID,
                     val sVars : Map[Symbol, List[SVarContainer[_]]],
                     private val removeObservers : Set[SVarActor.Ref],
                     private val creator : SVarActor.Ref,
                     createSelfSVar : Entity => SVar[Option[Entity]])
  extends EntityBase[Entity ] with Observability[Entity] with Accessibility[Entity]
{
  protected def asSelfType =
    this

  private var inRemoval =
    false

  override implicit val selfTypeTag: DataTag[SelfType] =
    types.Entity.typeTag

  protected final var selfSVar =
    createSelfSVar(this)

  lazy val toSVal =
    helper(description.typeDef.asConvertibleTrait)

  private def helper[T, B <: T](desc : ConvertibleTrait[T]) =
    desc(this.asInstanceOf[T])

  def this(e : Entity)(implicit creator : SVarActor) =
    this(e.description, e.id, e.sVars, e.removeObservers, e.creator, _ => e.selfSVar)

  def this(description : GeneralEntityDescription = new EntityDescription().desc)(implicit creator : SVarActor) =
    this(description, java.util.UUID.randomUUID(), Map(), Set(creator.self), creator.self,
      (e : Entity) => SVarImpl(SelfDesc(Some(e)), Now))

  def getSimpleName = description.path.map(_.name).mkString(" / ")

  //svar access
  override protected def access[T](c: ConvertibleTrait[T], actorContext : EntityUpdateHandling, filter : StateParticle[T] => Boolean = (_ : StateParticle[T]) => true) =
    ( f : AnnotatedMap[T] => Any) => accessMostRecent(actorContext) {
      e => f apply filterSVars(c, e.sVars.getOrElse(c.sVarIdentifier, Nil)).map(_.as(c).toAnnotatedMapEntry ).filter(x => filter(x._2)).toMap
    }


  override protected def handleNewValue[T](c: SValBase[T, _ ], timeStamp : Time, handler : SelfType => Any, bufferMode : BufferMode)
                                          (implicit actor: EntityUpdateHandling){
    doIfSelf( update(addOrUpdate(_, c, timeStamp, actor, bufferMode), handler, actor))({ HandleEntityUpdate(this, c.asSVal)}, handler)
  }

  def remove[T](c : ConvertibleTrait[T], handler : SelfType => Any = _ => {})(implicit actor : EntityUpdateHandling){
    doIfSelf( update(remove(_, c), handler, actor) )({ HandleEntityRemove(this, c) }, handler )
  }

  //instant access (may be not the most recent version of the entity)
  def containsSVars(c : ConvertibleTrait[_]) =
    sVars.get(c.sVarIdentifier).collect{
      case y => c.annotations.isEmpty || y.exists(z => c.annotations.forall(z.annotations.contains))
    }.getOrElse(false)

  // aspect related access
  def enableAspect(asp : EntityAspect, handler : Boolean => Unit = b => {})(implicit context : WorldInterfaceHandling){
    if (description.aspects.exists(_.aspectType equals asp.aspectType))
      context.enableAspect(this, asp, handler)
  }

  def disableAspect(asp : EntityAspect, handler : Boolean => Unit = b => {})(implicit context : WorldInterfaceHandling){
    if (description.aspects.exists(_.aspectType equals asp.aspectType))
      context.disableAspect(this, asp, handler)

  }

  //Entity Related access
  def observe(handler : (Entity, Time) => Unit, ignoredWriters : Set[SVarActor.Ref])(implicit actorContext : SVarActor) : java.util.UUID =
    actorContext match {
      case updateHandler : EntityUpdateHandling => updateHandler.updateObserve(this, selfSVar, (x : SelfType) => handler(x, Now))
      case _ => selfSVar.observe(_.collect{ case value => handler(value, Now)}, ignoredWriters)
    }

  def get(at : Time, accessMethod : AccessMethod)(handler: ContentType[Entity] => Unit)(implicit actorContext: SVarActor){
    accessMostRecent(handler(_, at)) //TODO
  }

  def getAllStateParticles(implicit context : EntityUpdateHandling) =
    getMostRecent(context).sVars.flatMap{
      tuple => tuple._2.map(x => x.asParticleInfo(tuple._1))
    }

  def getSVars[T](out : ConvertibleTrait[T])(implicit context : EntityUpdateHandling) =
    filterSVars(out, getMostRecent(context).sVars.getOrElse(out.sVarIdentifier, Nil)).map( x => x.annotations -> x.as(out).svar )

  def addRemoveObservers(observers : Set[SVarActor.Ref], handler : Entity => Any = _ => {})(implicit actorContext : SVarActor){
    doIfSelf( e => handler(e.addObservers(observers).getOrElse(this)))( HandleObserverUpdate(this, observers), handler )
  }

  def addRemoveObserver(observer : SVarActor.Ref)( handler : Entity => Any = _ => {})(implicit actorContext : SVarActor){
    addRemoveObservers(Set(observer), handler)
  }

  def remove()(implicit actor : SVarActor){
    doIfSelf{ e =>
      inRemoval = true
      selfSVar.get {
        _.collect {
          case entity => entity.removeObservers.foreach(sendRemovalMsg(_)(actor.self))
        }.getOrElse {
          e.removeObservers.foreach(sendRemovalMsg(_)(actor.self))
        }
      }
      setSelf(None, Now)
    }(HandleRemoveEntity(this))
  }

  def onUpdate(handler : SelfType => Any)(implicit actorContext : EntityUpdateHandling) : java.util.UUID =
    actorContext.addInternalUpdater(this, selfSVar, handler)


  def ignore(id : java.util.UUID)(implicit actorContext : EntityUpdateHandling) {
    actorContext.removeInternalUpdater(selfSVar, id)
  }

  private[core] def ignore()(implicit context : SVarActor){
    selfSVar.ignore()
  }

  protected[entity] def injectSVar[T](sVarName : Symbol, sVar : StateParticle[T],
                                      info : ConvertibleTrait[T], annotations : Annotation*)
                                     (handler : SelfType => Unit)(implicit actor : SVarActor){
    setSelf(Some(setSVars(insertParticle(sVarName, sVar, info.addAnnotations(annotations.toSet), sVars))), Now)
    selfSVar.get(x => handler(x.getOrElse(this)))
  }

  // private methods
  private def filterSVars(c : ConvertibleTrait[_], svars : List[SVarContainer[_]]) =
    svars.filter{ x => c.annotations.isEmpty || c.annotations.forall(x.annotations.contains) }

  private def setSVars(newSVars : Map[Symbol, List[SVarContainer[_]]]) =
    new Entity(description, id, newSVars, removeObservers, creator, _ => selfSVar)

  private def addObservers(observers : Set[SVarActor.Ref])(implicit actorContext : SVarActor) =
    if (inRemoval){
      observers.foreach(sendRemovalMsg(_)(actorContext.self))
      Some(this)
    } else
      setSelf(Some(new Entity(description, id, sVars, removeObservers ++ observers, creator, (_ : Entity) => selfSVar)), Now)

  private def sendRemovalMsg(receiver : SVarActor.Ref)(implicit sender: SVarActor.Ref){
    receiver ! RemoveEntityMessage(this)
  }

  private def prepareParticle[B](c : SValBase[_, B], timeStamp : Time, actor : SVarActor, bufferMode : BufferMode) =
    if (c.typedSemantics.asConvertibleTrait.isSValDescription)
      c.asSVal.typedSemantics.asConvertibleTrait -> c.asSVal
    else
      c.asSVal.typedSemantics.asConvertibleTrait -> SVarImpl.apply(c.asSVal, timeStamp, bufferMode)(actor)

  private def addOrUpdate[T, B <: T](in : Map[Symbol, List[SVarContainer[_]]], c : SValBase[T, B], timeStamp : Time, actor : SVarActor, bufferMode : BufferMode) =
    in.getOrElse(c.typedSemantics.sVarIdentifier, Nil).find( _.annotations equals c.typedSemantics.annotations) match {
      case Some(thing : SVarContainer[T]) =>
        thing.svar.set(c.value, timeStamp)(actor)
        false -> in
      case Some(thing) =>
        throw new Exception("impossible type mismatch")
      case None =>
        if (!c.typedSemantics.isCoreType) {
          val (typeInfo, particle) = prepareParticle(c.asBaseType, timeStamp, actor, bufferMode)
          true -> insertParticle(c.typedSemantics.sVarIdentifier, particle, typeInfo, in)
        } else {
          val (typeInfo, particle) = prepareParticle(c, timeStamp, actor, bufferMode)
          true -> insertParticle(c.typedSemantics.sVarIdentifier, particle, typeInfo, in)
        }
    }

  private[entity] def insertParticle[T](sVarName : Symbol, toInsert : StateParticle[T], c : ConvertibleTrait[T], in : Map[Symbol, List[SVarContainer[_]]]) =
    in.updated(sVarName, SVarContainer( toInsert, c ) :: in.getOrElse(sVarName, Nil) )

  private def remove[T](in : Map[Symbol, List[SVarContainer[_]]], c : ConvertibleTrait[T]) : (Boolean, Map[Symbol, List[SVarContainer[_]]]) =
    in.getOrElse(c.sVarIdentifier, Nil) match {
      case Nil => false -> in
      case oldValues => oldValues.filterNot(_.annotations equals c.annotations) match{
        case Nil => true -> (in - c.sVarIdentifier)
        case newValues if newValues.size == oldValues.size => false -> in
        case newValues => true -> in.updated(c.sVarIdentifier, newValues)
      }
    }

  private def doIfSelf(doIf : Entity => Unit )(msg : => Any, handler : SelfType => Any = _ => {})(implicit actor : SVarActor){
    if (creator == actor.self) accessMostRecent(doIf)(actor) else actor.ask[SelfType](creator, msg)( handler(_) : Unit)
  }

  //convenience
  private def getMostRecent(actorContext : EntityUpdateHandling) : Entity =
    actorContext.get(selfSVar).getOrElse(this)

  private def accessMostRecent(actorContext : EntityUpdateHandling)(handler : Entity => Unit){
    actorContext.get(selfSVar) match {
      case Some (thing) => handler (thing)
      case None => accessMostRecent (handler) (actorContext)
    }
  }

  private def accessMostRecent(handler : Entity => Unit)(implicit actorContext : SVarActor){
    selfSVar.get(x => handler(x.getOrElse(this)))
  }


  protected def update(createSVars : Map[Symbol, List[SVarContainer[_]]] => (Boolean, Map[Symbol, List[SVarContainer[_]]]),
                       handler : SelfType => Any, actor : EntityUpdateHandling)(oldVal : Entity){
    accessMostRecent(actor){ e =>
        val (updated, newSVars) = createSVars(e.sVars)
        if (updated) {
          val newVal = setSelf(Some(e.setSVars(newSVars)), Now)(actor)
          actor.updateInternalRep(selfSVar.id, newVal.getOrElse(this))
          handler(newVal.getOrElse(asSelfType))
        } else
          handler(e)
      }
  }

  override def hashCode(): Int = id.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case that : Entity => that.id == this.id
    case _ => false
  }

  override def toString: String = description.path.map(_.name).mkString(" / ") + " / " + id + (
    if (sVars.nonEmpty)
      " with StateParticles " + sVars.map(tuple => tuple._1.name -> tuple._2.mkString(", ")).mkString(", ")
    else
      " without any StateParticles"
    )
}


/**
 * implementation of the NewSVarContainerBase trait
 * For internal use
 */
protected object SVarContainer{
  type TypedStateParticle[T] = (ConvertibleTrait[T], StateParticle[T])
}

protected case class SVarContainer[T]( svar : StateParticle[T], info : ConvertibleTrait[T]){
  def convertSVar[O]( out : ConvertibleTrait[O], reverter : Option[IReverter[O, _]] = None) : StateParticle[O] =
    info.createConverter(svar, out, reverter)

  def asParticleInfo(identifier : Symbol) =
    StateParticleInfo[T](identifier, annotations, svar, info)

  def as[O]( out : ConvertibleTrait[O], reverter : Option[IReverter[O, _]] = None) : SVarContainer[O] = {
    val outWithCorrectAnnotations = out.setAnnotations(annotations)
    SVarContainer(convertSVar(outWithCorrectAnnotations, reverter), outWithCorrectAnnotations)
  }

  def toAnnotatedMapEntry : (MapKey[T], StateParticle[T]) =
    MapKey(info, AnnotationSet(annotations.toSeq :_*)) -> svar

  def asTyped : SVarContainer.TypedStateParticle[T] =
    info -> svar

  override def toString: String =
    info.toString

  final def annotations =
    info.annotations
}
