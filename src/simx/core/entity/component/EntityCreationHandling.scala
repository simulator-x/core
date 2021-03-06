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

package simx.core.entity.component

import simx.core.entity.Entity
import simx.core.entity.description._
import simx.core.svaractor.{TimedRingBuffer, SVarActor}
import simx.core.entity.typeconversion._
import simx.core.ontology._
import simx.core.svaractor.TimedRingBuffer.{BufferMode, Now}
import simx.core.worldinterface.{WorldInterfaceActor, WorldInterfaceHandling}
import simx.core.worldinterface.naming.NamingAspect
import scala.reflect.ClassTag
import simx.core.ontology.types
import simx.core.entity.description.NoInitialValuesException
import simx.core.entity.typeconversion.Own
import simx.core.entity.description.Providing
import simx.core.entity.description.ResolveRequirementsException
import simx.core.entity.typeconversion.Provide
import simx.core.entity.description.DoubleDefinitionException
import simx.core.helper.NewExecScheme
import simx.core.worldinterface.naming.NameIt
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling


/**
 * User: stephan_rehfeld
 * User: dwiebusch
 * Date: 12.12.11
 * Time: 08:44
 */

trait EntityCreationHandling extends SVarActor with WorldInterfaceHandling with EntityUpdateHandling{
  /**
   *  creates a new entity described by this description and executes the given handler afterwards
   * @param handler the handler executed after the creation process is finished
   */
  final protected[core] def realize(desc: GeneralEntityDescription)(handler: Entity => Any){
    createEntity(desc, handler)
  }

  final protected[core] def realize(desc: SpecificEntityDescription)(handler: Entity => Any){
    realize(desc.desc)(handler)
  }

  protected var printDoubleWarnings = true

  private class CreationStatus( val description   : GeneralEntityDescription,
                                val entityAspects : Seq[EntityAspect],
                                createType        : (Entity, SVarActor)  => Entity,
                                theHandler        : Option[Entity  => Any])  extends Serializable
  {
    private var result : Option[Entity] = None
    val theEntity           = new Entity(description)
    val values              = new SValSet
    val id                  = java.util.UUID.randomUUID()
    var openGetDepRequests  = List[SVarActor.Ref]()
    val ownerMap            = collection.mutable.Map[Symbol, SVarActor.Ref]()
    val bufferModeMap       = collection.mutable.Map[ConvertibleTrait[_], BufferMode]()
    val openGetInitRequests = collection.mutable.Map[SVarActor.Ref, Set[ConvertibleTrait[_]]]()
    val constSet            = collection.mutable.Set[SVal[_,_,_,_]]()
    val componentsCache     = collection.mutable.Map[GroundedSymbol, List[SVarActor.Ref]](
      Symbols.entityCreation -> List(self),
      Symbols.component -> List(WorldInterfaceActor.self),
      NamingAspect.componentType -> List(WorldInterfaceActor.self)
    )
    var sortedDeps          = List[TripleSet]()
    val storedDeps          = collection.mutable.Map[SVarActor.Ref, Map[EntityAspect, Set[Dependencies]]]()
    var subElements         = Seq[SValBase[Entity, _ <: Entity]]()
    def handle() =
      theHandler.collect{ case h => h(result.getOrElse(throw new Exception(""))) }

    def createFrom(e : Entity)(implicit creator : SVarActor) : Entity = {
      result = Some(createType(e, creator))
      result.get
    }
  }

  private val creationStatusMap = new collection.mutable.HashMap[java.util.UUID, CreationStatus]()

  case class HasSubEntity(override val componentType : GroundedSymbol, override val targets : List[Symbol])
    extends EntityAspect(componentType, Symbols.subElement, targets){
    override def getCreateParams: NamedSValSet = NamedSValSet(aspectType)
    override def getProvidings: Set[ConvertibleTrait[_]] = Set()
    override def getFeatures: Set[ConvertibleTrait[_]] = Set()
  }

//  private def analyzeSubEntities(status : Seq[EntityAspect]) : Set[EntityAspect] =
//    status.filter(_.componentType == Symbols.entityCreation).flatMap{
//      _.getCreateParams.getAllValuesFor(simx.core.ontology.types.EntityDescription).flatMap{
//        _.aspects.map(x => x.componentType -> x.targets)
//      }
//    }.toSet.map((x : (GroundedSymbol, List[Symbol])) => HasSubEntity(x._1, x._2))

  private def createEntity( ed : GeneralEntityDescription, handler : Entity => Any ){
    var additionalAspects = Set[EntityAspect]()//analyzeSubEntities(ed.aspects)
    if (!ed.aspects.exists(_.getProvidings.contains(NamingAspect.providing)))
      additionalAspects = additionalAspects + NameIt(ed.path.last.name)

    val e = if (additionalAspects.isEmpty) ed else ed.copy(ed.aspects ++ additionalAspects)

    val status = new CreationStatus(e, e.aspects, e.createType, Some(handler))
    creationStatusMap.update(status.id, status)
    var numberOfAnswers = 0
    val entityAspects = status.entityAspects.filterNot(aspect => status.componentsCache.contains(aspect.componentType))

    if( entityAspects.isEmpty )
      startCreation(status)
    else
      for( entityAspect <- entityAspects )
        handleComponents( entityAspect.componentType )( handleComponentList(status, entityAspect) )

    def handleComponentList(status : CreationStatus, aspect : EntityAspect, iteration : Int = 0)
                           (list : List[(Symbol, SVarActor.Ref)]){
      if (list.isEmpty || !aspect.targets.forall(t => list.exists(_._1 == t))) addJobIn(500){
        if (iteration == 5)
          println("waiting for components of type " + aspect.componentType.value)
        else if (iteration > 50)
          throw new Exception("Component of type " + aspect.componentType.value + " not found ")
        else
          handleComponents(aspect.componentType)(handleComponentList(status, aspect, iteration + 1))
      } else {
        val componentsList = if (aspect.targets.nonEmpty) list.filter(x => aspect.targets.contains(x._1)) else list
        status.componentsCache.update( aspect.componentType, componentsList.map(_._2))
        numberOfAnswers = numberOfAnswers + 1
        if( numberOfAnswers == entityAspects.size )
          startCreation(status)
      }
    }
  }

  // some typedefs
  private type TripleSet = (SVarActor.Ref, EntityAspect, Set[ConvertibleTrait[_]])
  private type Triple = (SVarActor.Ref, EntityAspect, Dependencies)

  /**
   *  Creates a map (T, Actor) by applying the given partial function to all provides and requires of each aspect.
   *        The function's result (if it can be applied) is added as key to the map (value = the associated component).
   * @param func the partial function to be appplied
   * @return a map with func's results as keys and associated components as values
   */
  private def extractPar[T]( status : CreationStatus, func : PartialFunction[ProvideAndRequire[_, _], Option[T]],
                             resultMap : collection.mutable.Map[T, SVarActor.Ref]) {
    val fallback = { case _ => None } : PartialFunction[ProvideAndRequire[_, _], Option[T]]
    status.entityAspects.foreach{ aspect =>
      val theComponent = status.componentsCache(aspect.componentType).head
      aspect.overrides.foreach{
        elem => func.orElse(fallback).apply(elem).collect{ case x => resultMap.update(x, theComponent) }
      }
    }
  }

  //start the entity creation process by sending get dependencies messages
  private def startCreation( status : CreationStatus ) {
    debug("creating " + status.entityAspects.map(_.getCreateParams.semantics.value).mkString(", "))
    extractPar(status, { case Own(c) => Some(c.sVarIdentifier) }, status.ownerMap)
    status.entityAspects.foreach{ aspect =>
      status.bufferModeMap ++= aspect.getBufferSettings
      status.componentsCache( aspect.componentType ).headOption.foreach { component =>
        if (aspect.componentType == Symbols.entityCreation)
          self ! GetDependenciesAns( status.id, aspect.getDependencies, aspect )
        else
          component ! GetDependenciesMsg( status.id, aspect )
        status.openGetDepRequests = component :: status.openGetDepRequests
      }
    }
  }

  //process dependencies
  addHandler[GetDependenciesAns]{ msg =>
    val status = creationStatusMap(msg.requestId)
    status.storedDeps.update(msg.sender,
      status.storedDeps.getOrElse(
        msg.sender, Map[EntityAspect,  Set[Dependencies]]()) + (msg.asp -> mergeProvidings(msg.asp, msg.deps))
    )
    status.openGetDepRequests = status.openGetDepRequests.filterNot(_ == msg.sender) :::
      status.openGetDepRequests.filter(_ == msg.sender).drop(1)
    if (status.openGetDepRequests.isEmpty){
      val tmp = collection.mutable.Map[ConvertibleTrait[_], SVarActor.Ref]()
      extractPar[ConvertibleTrait[_]](status, { case Provide(c) => Some(c.from) }, tmp )
      filterProvidings(status.storedDeps, tmp, status.storedDeps )
      status.sortedDeps = sortDependencies(status.storedDeps)
      checkForDoubles(status.sortedDeps, status.ownerMap)
      setOwners(status)
      requestInitialValues(status)
    }
  }

  /**
   *  adds the providings from the given entity aspect to the given set of dependencies, if they are not contained
   *        already.
   * @param a the aspect to be merged
   * @param ds the given set of dependencies
   * @return the merged set
   */
  private def mergeProvidings( a : EntityAspect, ds : Set[Dependencies] ) : Set[Dependencies] = {
    val toAdd = a.getProvidings.filter(p => ds.forall(!_.providings.objects.contains(p)))
    if (toAdd.nonEmpty) ds + Dependencies(Providing(toAdd.toSeq : _* )) else ds
  }

  /**
   *  filters the dependencies provided by each component and adds/removes user defined providings (isProvided)
   * @param m the map to be filtered
   */
  private def filterProvidings(m : collection.mutable.Map[SVarActor.Ref, Map[EntityAspect, Set[Dependencies]]],
                               providings : collection.mutable.Map[ConvertibleTrait[_], SVarActor.Ref],
                               resultMap : collection.mutable.Map[SVarActor.Ref, Map[EntityAspect, Set[Dependencies]]]) =
    m.foreach{ tuple =>
      resultMap.update(tuple._1,  tuple._2.map{ iTuple =>
        val correctedDeps = handleDeps(tuple._1, iTuple._2, providings)
        if (correctedDeps.isEmpty) iTuple else iTuple._1 -> correctedDeps
      } )
    }

  /**
   *  applies the handle dep function to each dependency in the given set, resulting in a set of dependencies
   *        from which all providings that were overridden by the user are removed
   * @return the cleaned set
   */
  private def handleDeps(actor : SVarActor.Ref, dep : Set[Dependencies], providings : collection.mutable.Map[ConvertibleTrait[_], SVarActor.Ref]) : Set[Dependencies] = {
    val retVal = dep.foldLeft(dep.empty){
      (set, elem) => handleDep(actor, elem, providings).collect{ case f => set + f }.getOrElse(set)
    }
    val missing = providings.filterNot( p => p._2 != actor || retVal.exists(_.providings.objects.exists(_.sVarIdentifier == p._1)) )
    if (missing.isEmpty) retVal else retVal + Dependencies(Providing(missing.keys.toSeq :_*))
  }

  /**
   *  checks for each providing in the given dependencies if it shall be provided by the given actor. If it shall
   *        be provided by another actor, it is removed from the dependency
   * @return an option containing the cleaned dependencies, if it still contains providings, None otherwise
   */
  private def handleDep( actor : SVarActor.Ref, dep : Dependencies, providings : collection.mutable.Map[ConvertibleTrait[_], SVarActor.Ref] ) : Option[Dependencies] = {
    val filtered = dep.providings.objects.filter( p => providings.getOrElse(p, actor) == actor )
    if (filtered.isEmpty) None else Some(Dependencies(Providing(filtered : _*), dep.requirings))
  }

  /**
   *  creates the buildorder from the given dependency map
   * @param deps the input dependency map
   * @return a ordered list of type TripleSet which is sorted according to the given dependencies
   */
  private def sortDependencies(deps : collection.mutable.Map[SVarActor.Ref, Map[EntityAspect, Set[Dependencies]]]) : List[TripleSet] = {
    val tmp = deps.flatMap( elem => elem._2.flatMap( x => x._2.map( (elem._1, x._1, _) ).toList ) ).toList
    val (pre, toSort) = tmp.partition( _._3.requirings.objects.isEmpty )
    merge( pre ::: sort(toSort, pre.flatMap(_._3.providings.objects).toSet) )
  }

  /**
   *  sorts the given list of triples and creates an ordered list of triples that is sorted by dependencies
   * @param toSort the list of triples to be sorted
   * @param known the set of convertible traits which is already provided
   * @return the ordered list
   */
  private def sort(toSort : List[Triple], known : Set[ConvertibleTrait[_]]) : List[Triple] = toSort match {
    case Nil  => Nil
    case list =>
      val (found, remaining) = list.partition( (_ : Triple)._3.requirings.objects.forall( known.contains ) )
      if (found.isEmpty) throw ResolveRequirementsException( remaining.map{ _._2 } )
      found ::: sort(remaining, found.foldLeft(known){ _ ++ _._3.providings.objects } )
  }

  /**
   *  merges elements of the given buildorder, when possible
   * @param list the buildorder
   * @param currentSet the set of providings which has been collected previously (for internal usage)
   * @return the merged list
   */
  private def merge(list : List[Triple], currentSet : Set[ConvertibleTrait[_]] = Set()) : List[TripleSet] = list match{
    case Nil => Nil
    case (actor, asp, dep) :: Nil  => (actor, asp, currentSet ++ dep.providings.objects) :: Nil
    case head :: tail if cont(head, tail.head) => merge(tail, currentSet ++ head._3.providings.objects)
    case (actor, asp, dep) :: tail => (actor, asp, currentSet ++ dep.providings.objects ) :: merge(tail)
  }

  /**
   *  checks if the given actor is responsible for the given next triple and if the requirings of the next triple
   *        is a subset of the requirings of the given dependencies
   * @param current the current triple
   * @param next the next triple
   * @return true if both conditions hold
   */
  private def cont(current : Triple, next : Triple) : Boolean =
    current._1 == next._1 && current._2 == next._2 &&
      next._3.requirings.objects.forall(current._3.requirings.objects.contains)

  /**
   *  throws a DoubleDefinitionException, if there are multiple occurrences of the same ConvertibleTrait in the
   *        given list of triple sets
   * @param l the list to be checked for doubles
   */
  private def checkForDoubles( l : List[TripleSet], owners : collection.mutable.Map[Symbol, SVarActor.Ref] ) =
    l.foldLeft(Set[ConvertibleTrait[_]]()){ (set, elem) =>
      if (elem._3.forall{ x => x.getBase.equals(types.Entity) || !set.contains(x) || owners.contains(x.sVarIdentifier)})
        set ++ elem._3
      else  throw generateDoubleDefinitionException(l)
    }

  /**
   * generates exception if a list of triple sets is defined incorrectly
   * @param l the list of triple sets which is defined incorrectly
   * @return the generated exception
   */
  private def generateDoubleDefinitionException( l : List[TripleSet] ) = {
    val double = l.foldLeft(Map[ConvertibleTrait[_], List[EntityAspect]]()){
      (map, elem) => elem._3.foldLeft(map){ (m, e) => m.updated(e, elem._2 :: m.getOrElse(e, Nil)) }
    }.filter( _._2.length > 1 )
    DoubleDefinitionException(double.map(t => t._1 + " in\n\t\t - " + t._2.mkString("\n\t\t - ")).mkString("\n\t"))
  }

  /**
   *  updates the ownermap according to the information from the given list of triplesets
   * @param status the creation status
   */
  private def setOwners( status: CreationStatus ) {
    status.sortedDeps.foreach{
      c => c._3.foreach{
        e => status.ownerMap.update(e.sVarIdentifier, status.ownerMap.getOrElse(e.sVarIdentifier, c._1))
      }
    }
  }

  /**
   *  sends a GetInitialValuesMsg to the first actor in the given build order and stores its tail in the
   *        variable sortedDeps
   * @param status the creation status
   */
  private def requestInitialValues( status : CreationStatus ) {
    status.sortedDeps match {
      case Nil => createSVars(status)
      case (actor, aspect, providings) :: tail =>
        if (aspect.componentType == Symbols.entityCreation)
          selfRequestInitialValues(aspect, status.theEntity){ svals =>
            status.subElements ++= svals
            status.sortedDeps  = tail
            requestInitialValues(status)
          }
        else {
          status.openGetInitRequests.update(actor, providings)
          actor ! GetInitialValuesMsg(status.id, providings.toSet, aspect, status.theEntity, new SValSet(status.values) )
        }
        status.sortedDeps = tail
    }
  }

  // ------------------------------
  // SubElement Creation
  // ------------------------------
  private def selfRequestInitialValues(aspect: EntityAspect, e: Entity)(provideInitialValues : Seq[SValBase[Entity, Entity]] => Unit) {
    if (aspect.componentType != Symbols.entityCreation)
      throw new Exception("error: unknown aspect for sub-entity creation: " + aspect)
    else
      aspect.getCreateParams.getAllValuesFor(simx.core.ontology.types.EntityDescription) match {
        case head :: tail => create(e, head, tail).exec{ set => provideInitialValues(set.toSeq)  }
        case Nil => provideInitialValues(Seq())
      }
  }

  private def create( parent : Entity, head : SpecificDescription,
                      tail : List[SpecificDescription] )  =
    tail.foldLeft(NewExecScheme[SValBase[Entity, Entity]](sValFrom(parent, head))){ _ and sValFrom(parent, _) }

  private def sValFrom[T <: Entity : ClassTag]( parent : Entity,  desc : GeneralEntityDescription )  =
    ( handler : SValBase[Entity, Entity] => Any ) => realizeAsSVal(desc, parent, handler)

  private def realizeAsSVal[T <: Entity : ClassTag]( desc : GeneralEntityDescription, parent : Entity, handler : SVal.SValType[Entity] => Any ){
    val newAspects = desc.aspects
    newAspects.foreach(_.setParent(parent))
    val newDescription = desc.copy( newAspects )
    newDescription.realize( (x : Entity ) => handler(desc.typeDef.asConvertibleTrait.apply(x)))
  }
  // ------------------------------
  // End of SubElement Creation
  // ------------------------------

  //process initial values
  addHandler[GetInitialValuesAns]{ msg =>
    def remap[T](desc : ConvertibleTrait[T]) : List[SVal.SValType[_]] =
      msg.initialValues.getAllSValsFor(desc).map(x => desc(x.value))

    val status = creationStatusMap(msg.id)
    if (printDoubleWarnings) msg.initialValues.filterNot{
      p => status.storedDeps.get(msg.sender).collect{
        case x => x.values.exists(_.exists(_.providings.objects.exists(_.sVarIdentifier == p._1)))
      }.getOrElse(true)
    }.foreach( doubleVal => println(doubleVal._1 + " was provided although it was not requested") )
    status.values ++= SValSet(status.openGetInitRequests(sender()).flatMap(remap(_)).toSeq :_*)
    requestInitialValues(status)
  }

  //actually create the svars
  /**
   *  generates a list of actor provideconversion-set tuples. the actor in the head tuple of this list will be
   *        sent an CreateSVarsMsg (containing the entity to be filled) that will be passed around. Actor.self will be
   *        the last receiver of that message
   */
  private def createSVars( status : CreationStatus ) {
    val providings = status.storedDeps.flatMap(_._2.flatMap(_._2.flatMap(_.providings.objects)))
    val requirings = status.values.map(_._2.head.typedSemantics)
    checkValidity(status, requirings, providings ++ status.subElements.map(_.typedSemantics))
    status.values.values.flatten.foldLeft(Map[SVarActor.Ref, List[(ProvideConversionInfo[_,_], BufferMode)]]()){
      (map, value) =>
        val typeInfo = value.typedSemantics
        val owner = status.ownerMap(typeInfo.sVarIdentifier)
        val list = map.getOrElse(owner, List[(ProvideConversionInfo[_, _], BufferMode)]())
        val bufferMode = status.bufferModeMap.getOrElse(value.asProvide.wrapped.to, TimedRingBuffer.defaultMode)
        map.updated(owner, value.asProvide.wrapped -> bufferMode :: list)
    }.toList :+ (self -> Nil) match {
      case (`self`, Nil) :: Nil  => finalizeCreation(status.id, status.theEntity)
      case (actor, list) :: tail => actor ! CreateSVarsMsg(status.id, list, status.theEntity, tail)
      case Nil => // this should never happen since we added `creationContext`
    }
  }

  /**
   *  checks if all SVars that the aspects require will be provided. Throws an Exception if this is not the case
   * @param requirings the given requirings
   * @param providings the given providings
   */
  private def checkValidity( status : CreationStatus, requirings : Iterable[TypeInfo[_, _]],
                             providings : Iterable[TypeInfo[_, _]] ) {
    val missing = requirings.filter( p => !providings.exists(_.sVarIdentifier == p.sVarIdentifier) )
    if (missing.nonEmpty) throw NoInitialValuesException(missing.toSeq, status.ownerMap, status.entityAspects)
  }

  addHandler[SVarsCreatedMsg]{
    msg => finalizeCreation(msg.id, msg.entity)
  }

  private def finalizeCreation( id : java.util.UUID, e : Entity ) {
    val status = creationStatusMap.remove(id).get

    def handler(result: Entity) {
      val obs = status.entityAspects.flatMap { asp => status.componentsCache(asp.componentType) }.toSet
      result.addRemoveObservers(obs + WorldInterfaceActor.self + self, { entityWithObservers =>
        val result = status createFrom entityWithObservers
        val finalPath = status.description.path :+ Symbol(result.id.toString)
        onNextEntityRegistration(finalPath) { entity =>
          val componentsToNotify = status.entityAspects.filterNot(_.componentType == Symbols.entityCreation)
          var numberOfCompleteRequests = componentsToNotify.size
          componentsToNotify.foreach {
            asp => status.componentsCache(asp.componentType).foreach {
              waitUntilProcessed(_, EntityCompleteMsg(asp, entity)){
                numberOfCompleteRequests -= 1
                if (numberOfCompleteRequests <= 0){
                  info("completed " + status.entityAspects.map(_.getCreateParams.semantics.value).mkString(", "))
                  status.handle()
                }
              }
            }
          }
        }

        if (finalPath.head == Symbols.component.value.toSymbol)
          result.get(types.Component).head {
            waitUntilProcessed(_, FinalizeComponentConfigMsg(result)) {
              registerEntity(finalPath, result)
            }
          }
        else
          registerEntity(finalPath, result)
      })
    }

//    status.description.additionalProperties.foreach(_._2.foreach{sVal =>
//      def toConst[T, B <: T](semanticValue: SValBase[T,B]) =
//        semanticValue.typedSemantics.asConvertibleTrait.asConst.apply(semanticValue.value.asInstanceOf[B])
//      e.set(toConst(sVal))
//    })

    val subElements = status.subElements
    var injectFunc = subElements.map{ _.asProvide.wrapped }.foldRight(handler _){
      (toAdd, hnd) => toAdd.injectSVar(_, Now, TimedRingBuffer.defaultMode)(hnd)
    }

    def toConst[T, B <: T](semanticValue: SValBase[T,B]) =
      semanticValue.typedSemantics.asConvertibleTrait.asConst.apply(semanticValue.value.asInstanceOf[B])

    val additionalProperties = status.description.additionalProperties.flatMap(_._2).toSeq

    injectFunc = additionalProperties.foldRight(injectFunc){
      (addProp, hnd) => e : Entity  => {
        e.set(toConst(addProp), hnd)
      }
    }

    injectFunc = subElements.foldRight(injectFunc){ (a, b) =>
      e : Entity => e.set( (
      if(a.value.description.typeDef.annotations.isEmpty)
        types.HasPart
      else
        types.HasPart.setAnnotations(a.value.description.typeDef.annotations)
      ) -> a.value,
        (x : Entity) => { a.value.set(types.PartOf -> e, (_ : Entity) => b(x)) }

        )
    }

    injectFunc(e)
  }
}
