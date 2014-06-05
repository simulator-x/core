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
import simx.core.svaractor.SVarActor
import simx.core.entity.typeconversion._
import simx.core.component.ComponentHandling
import simx.core.ontology.{GroundedSymbol, types}
import simx.core.entity.typeconversion.{Own, Provide}


/**
 * User: stephan_rehfeld
 * Date: 12.12.11
 * Time: 08:44
 */

trait EntityCreationHandling extends SVarActor with ComponentHandling {
  protected final implicit val creationContext = this
  /**
   *  creates a new entity described by this description and executes the given handler afterwards
   * @param handler the handler executed after the creation process is finished
   */
  final protected[core] def realize[Type <: Entity](entityDescription: GeneralEntityDescription[Type])(handler: (Type) => Any){
    createEntity(entityDescription, handler)
  }

  private var creationStatusMap = Map[java.util.UUID, CreationStatus[_ <: Entity]]()

  private class CreationStatus[Type <: Entity]( val entityAspects : Seq[EntityAspect],
                                                createType        : Entity  => Type,
                                                theHandler        : Option[Type  => Any]){
    private var result : Option[Type] = None
    val theEntity           = new Entity
    val values              = new SValSet
    val id                  = java.util.UUID.randomUUID()
    var openGetDepRequests  = List[SVarActor.Ref]()
    var ownerMap            = Map[Symbol, SVarActor.Ref]()
    var componentsCache     = Map[GroundedSymbol,List[SVarActor.Ref]]()
    var sortedDeps          = List[TripleSet]()
    var storedDeps          = Map[SVarActor.Ref, Map[EntityAspect, Set[Dependencies]]]()
    def handle() =
      theHandler.collect{ case h => h(result.getOrElse(throw new Exception(""))) }

    def createFrom(e : Entity) : Entity = {
      result = Some(createType(e))
      result.get
    }
  }

  def createEntity[Type <: Entity]( e : GeneralEntityDescription[Type], handler : Type => Any ){
    val entityAspects = e.aspects
    val status = new CreationStatus(entityAspects, e.createType, Some(handler))
    creationStatusMap   = creationStatusMap + (status.id -> status)
    val numberOfAspects = entityAspects.size
    var numberOfAnswers = 0
    for( entityAspect <- entityAspects ) {
      componentForType( entityAspect.componentType, (componentsList) => {
        status.componentsCache = status.componentsCache + ( entityAspect.componentType -> componentsList.getOrElse(
          throw new Exception("did not find component " + entityAspect.componentType.value.toString)))
        numberOfAnswers = numberOfAnswers + 1
        if( numberOfAnswers == numberOfAspects )
          startCreation(status)
      })
    }
  }

  // some typedefs
  private type TripleSet = (SVarActor.Ref, EntityAspect, Set[ConvertibleTrait[_]])
  private type Triple = (SVarActor.Ref, EntityAspect, Dependencies)

  //! the handler which notifies creation observers via the EntityDescription object
  private val notifyObservers = (e : Entity , s : Seq[EntityAspect]) => {
    GeneralEntityDescription.notifyObservers( EntityConfiguration(e, s.map( toTuple _).toMap) ); e
  }

  /**
   *  creates a tuple of componentType and NamedSValList from a given EntityAspect
   * @param a the EntityAspect
   * @return the resulting tuple
   */
  private def toTuple(a : EntityAspect) : (Symbol, NamedSValSet) =
    a.componentType.value.toSymbol -> a.getCreateParams


  /**
   *  Creates a map (T, Actor) by applying the given partial function to all provides and requires of each aspect.
   *        The Functions result (if it can be applied) is added as key to the map (value = the associated component).
   * @param func the partial function to be appplied
   * @return a map with func's results as keys and associated components as values
   */
  private def extractPar[T]( status : CreationStatus[_], func : PartialFunction[ProvideAndRequire, Option[T]]) = {
    val fallback = { case _ => None } : PartialFunction[ProvideAndRequire, Option[T]]
    status.entityAspects.foldLeft(Map[T, SVarActor.Ref]()){
      (map, aspect) =>
        val theComponent = status.componentsCache(aspect.componentType).head
        aspect.overrides.foldLeft(map){
          (m, elem) => func.orElse(fallback).apply(elem).collect{ case x => m.updated(x, theComponent) }.getOrElse(m)
        }
    }
  }

  //start the entity creation process by sending get dependencies messages
  private def startCreation( status : CreationStatus[_] ) {
    debug("creating " + status.entityAspects.map(_.getCreateParams.semantics.value).mkString(", "))
    status.ownerMap = extractPar(status, { case Own(c) => Some(c.sVarIdentifier) })
    status.entityAspects.foreach{ aspect =>
      status.componentsCache( aspect.componentType ).headOption.foreach {
        c => {
          c ! GetDependenciesMsg( status.id, aspect )
          status.openGetDepRequests = c :: status.openGetDepRequests
        }
      }
    }
  }

  //process dependencies
  addHandler[GetDependenciesAns]{ msg =>
    val status = creationStatusMap(msg.requestId)
    status.storedDeps = status.storedDeps.updated(msg.sender,
      status.storedDeps.getOrElse(
        msg.sender, Map[EntityAspect,  Set[Dependencies]]()) + (msg.asp -> mergeProvidings(msg.asp, msg.deps))
    )
    status.openGetDepRequests = status.openGetDepRequests.filterNot(_ == msg.sender) :::
      status.openGetDepRequests.filter(_ == msg.sender).drop(1)
    if (status.openGetDepRequests.isEmpty){
      status.storedDeps = filterProvidings(status.storedDeps, extractPar(status, { case Provide(c) => Some(c.getSVarName) } ))
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
  private def filterProvidings(m : Map[SVarActor.Ref, Map[EntityAspect, Set[Dependencies]]], providings : Map[Symbol, SVarActor.Ref]) =
    m.foldLeft(m.empty){ (map, tuple) =>
      map.updated(tuple._1,  tuple._2.map{ iTuple => {
        val correctedDeps = handleDeps(tuple._1, iTuple._2, providings)
        if (correctedDeps.isEmpty) iTuple else (iTuple._1 -> correctedDeps)
      } } )
    }

  /**
   *  applies the handle dep function to each dependency in the given set, resulting in a set of dependencies
   *        from which all providings that were overridden by the user are removed
   * @return the cleaned set
   */
  private def handleDeps(actor : SVarActor.Ref, dep : Set[Dependencies], providings : Map[Symbol, SVarActor.Ref]) : Set[Dependencies] =
    dep.foldLeft(dep.empty){
      (set, elem) => handleDep(actor, elem, providings).collect{ case f => set + f }.getOrElse(set)
    }

  /**
   *  checks for each providing in the given dependencies if it shall be provided by the given actor. If it shall
   *        be provided by another actor, it is removed from the dependency
   * @return an option containing the cleaned dependencies, if it still contains providings, None otherwise
   */
  private def handleDep( actor : SVarActor.Ref, dep : Dependencies, providings : Map[Symbol, SVarActor.Ref] ) : Option[Dependencies] = {
    val filtered = dep.providings.objects.filter( p => providings.getOrElse(p.sVarIdentifier, actor) == actor )
    if (filtered.isEmpty) None else Some(Dependencies(Providing(filtered : _*), dep.requirings))
  }

  /**
   *  creates the buildorder from the given dependency map
   * @param deps the input dependency map
   * @return a ordered list of type TripleSet which is sorted according to the given dependencies
   */
  private def sortDependencies(deps : Map[SVarActor.Ref, Map[EntityAspect, Set[Dependencies]]]) : List[TripleSet] = {
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
  private def checkForDoubles( l : List[TripleSet], owners : Map[Symbol, SVarActor.Ref] ) =
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
  private def setOwners( status: CreationStatus[_] ) {
    status.ownerMap = status.sortedDeps.foldLeft(status.ownerMap){
      (m, c) => c._3.foldLeft(m){
        (oMap, e) => oMap.updated(e.sVarIdentifier, oMap.getOrElse(e.sVarIdentifier, c._1))
      }
    }
  }

  /**
   *  sends a GetInitialValuesMsg to the first actor in the given build order and stores its tail in the
   *        variable sortedDeps
   * @param status the creation status
   */
  private def requestInitialValues( status : CreationStatus[_] ) {
    status.sortedDeps match {
      case Nil => createSVars(status)
      case (actor, aspect, providings) :: tail =>
        val provideSet = providings.toSet.filter{
          p => status.ownerMap.get(p.sVarIdentifier).collect{ case x => x equals actor }.getOrElse(true)
        }
        actor ! GetInitialValuesMsg(status.id, provideSet, aspect, status.theEntity, new SValSet(status.values) )
        status.sortedDeps = tail
    }
  }

  //process initial values
  addHandler[GetInitialValuesAns]{
    msg =>
      val status = creationStatusMap(msg.id)
      msg.initialValues.filterNot{
        p => status.ownerMap.get(p._1).collect{case x => x equals msg.sender}.getOrElse(true)
      }.foreach( doubleVal => println(doubleVal._1 + " was provided although it was not requested"))
      status.values ++= msg.initialValues
      requestInitialValues(status)
  }

  //actually create the svars
  /**
   *  generates a list of actor provideconversion-set tuples. the actor in the head tuple of this list will be
   *        sent an CreateSVarsMsg (containing the entity to be filled) that will be passed around. Actor.self will be
   *        the last receiver of that message
   */
  private def createSVars( status : CreationStatus[_] ) {
    val providings = status.storedDeps.values.flatMap(_.flatMap(_._2.flatMap(_.providings.objects)))
    checkValidity(status, providings.toSeq, status.values.map(_._2.head.typedSemantics))
    status.values.values.flatten.foldLeft(Map[SVarActor.Ref, List[ProvideConversionInfo[_,_]]]()){
      (map, value) =>
        val typeInfo = value.typedSemantics.asConvertibleTrait
        val owner = status.ownerMap(typeInfo.sVarIdentifier)
        val list = map.getOrElse(owner, List[ProvideConversionInfo[_, _]]())
        map.updated(owner, value.asProvide.wrapped :: list)
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
  private def checkValidity( status : CreationStatus[_], requirings : Seq[ConvertibleTrait[_]], providings : Iterable[TypeInfo[_]] ) {
    val missing = requirings.filter( p => providings.find( _.sVarIdentifier == p.sVarIdentifier ).isEmpty )
    if (missing.nonEmpty) throw NoInitialValuesException(missing, status.ownerMap, status.entityAspects)
  }

  addHandler[SVarsCreatedMsg]{
    msg => finalizeCreation(msg.id, msg.entity)
  }

  protected def finalizeCreation( id : java.util.UUID, e : Entity) {
    val status = creationStatusMap( id )
    val entity = status.createFrom( e )
    val observers = status.entityAspects.flatMap{ a => status.componentsCache(a.componentType) }.toSet + self

    entity.setRemoveObservers(entity.getRemoveObservers ++ observers)
    status.entityAspects.foreach{ asp =>
      status.componentsCache( asp.componentType ).foreach{
        _ ! EntityCompleteMsg(asp, entity)
      }
    }

    notifyObservers(entity, status.entityAspects )
    status.handle()

    info("completed " + status.entityAspects.map(_.getCreateParams.semantics.value).mkString(", " ))
  }
}
