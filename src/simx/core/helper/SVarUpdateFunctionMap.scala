/*
  * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package simx.core.helper

import simx.core.entity.typeconversion.TypeInfo._
import simx.core.svaractor.TimedRingBuffer.Now
import simx.core.svaractor.semantictrait.base.BaseValueDescription

import scala.collection._
import simx.core.entity.Entity
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.description.{SVal, SValSet}
import simx.core.svaractor.{GetClosest, StateParticle, SVarActor, SVar}
import scala.reflect.ClassTag
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

/**
 *  A trait for updating component-internal entities and their associated svars using a hashmap.
 */
trait SVarUpdateFunctionMap extends SVarActor with EntityUpdateHandling {

  /**
   *  Stores a consume function for a svar.
   */
  protected class ChangeableConsume[T](var func: Option[T => Unit])

  /**
   *  Stores functions to update an SVar from the value of its associated internal entity and vice versa.
   * @see addSVarUpdateFunctions
   */
  protected case class GetAndSet[T](consumeSVar:  ChangeableConsume[T], var updateSVar: Option[() => Boolean])

  private val updateFunctionMap = mutable.WeakHashMap[StateParticle[_], GetAndSet[_]]()
  private val pausedFunctionMap = mutable.WeakHashMap[StateParticle[_], GetAndSet[_]]()

  /**
   *    Value changes by SVarActors contained in this set do not trigger consume functions via observe.
   *  @see    simx.core.svaractor.SVar.observe()
   */
  var ignoredWriters: immutable.Set[SVarActor.Ref] = immutable.Set()

  /**
   *    Requests the values of svars that belong to one entity and applies a handler when every value has arrived.
   *  Initially observe and get is called on all desired svars using the mechanisms of
   *          the SVarUpdateFunctionMap trait. Whenever a (new) svar value arrives at the calling actor,
   *          it is stored locally. If at least one value has arrived for every svar, the given handler is called.
   *          This handler can then use all requested values at once.
   *
   * @param e               The entity the contains the desired svars
   * @param toGet           Identifiers for all desired svars
   * @param handler         A function that processes the requested svar values.
   *                        It is called once all values have arrived.
   * @param keepRegistered  If true the svars are not ignored (SVar mechanism) and are not removed from the
   *                        SVarUpdateFunctionMap trait's machanisms. That way the observe's consume functions can be
   *                        changed without calling ignore. updateConsumeSVar or removeSVarUpdateFunctions can be used
   *                        to change consume functions or ignore svars. If none of the previous two methods is called
   *                        and keepRegistered was true the svars stay observed using a consume function that does nothing.
   */
  def collectSVars(e: Entity, toGet: ConvertibleTrait[_]*)
                  (handler: SValSet => Unit, keepRegistered: Boolean = false) {

    //Number of actual existing sVars in the entity that match toGet
    val targetSize = (for(ct <- toGet.toSeq) yield e.getSVars(ct).size).sum
    val retrievedValues = mutable.Map[StateParticle[_], SVal.SValType[_]]()//new SValSet()

    toGet.foreach((ct: ConvertibleTrait[_]) => {init(ct)})

    def init[T : ClassTag : DataTag](ct: ConvertibleTrait[T]) {
      e.getSVars(ct).foreach{
        sVar => addSVarUpdateFunctions(sVar._2, Some((x : T) => valueArrived[T](ct, sVar._2)(x)), None, useGet = true, Some(ct))
      }
    }

    def clean[T](ct: ConvertibleTrait[T]) {
      e.getSVars(ct).map{ sVar =>
        updateConsumeSVar(sVar._2, None)
        updateGetValueForSVar(sVar._2, None)
      }
    }

    def remove[T](ct: ConvertibleTrait[T]) {
      e.getSVars(ct).foreach( x => removeSVarUpdateFunctions(x._2))
    }

    def valueArrived[T](ct: ConvertibleTrait[T], sVar: StateParticle[T])(value: T) {
      implicit val classTag = ct.classTag
      retrievedValues.update(sVar, ct(value))
      if (retrievedValues.size == targetSize) done()
    }

    def done() {
      if(keepRegistered)
        toGet.foreach((ct: ConvertibleTrait[_]) => {clean(ct)})
      else
        toGet.foreach((ct: ConvertibleTrait[_]) => {remove(ct)})
      handler(new SValSet(retrievedValues.values.toSeq: _*))
    }
  }

  /**
   *    Adds two update functions to update an SVar from the value of its associated internal entity and vice versa.
   *          If there where already functions registered for this svar, the method will return and print a warning.
   * Passing None is equal to passing functions that do nothing.
   * @param svar            An SVar
   * @param consumeSVar     A function, that updates the internal entity associated with svar, using the svar observe mechanism.
   * @param getValueForSVar A function that retrieves the corresponding value from the internal entity associated with svar.
   * @param useGet          If true, additionally to observe, get is called once on svar using consumeSVar.
   *                        This way the svars current value guaranteed to be retreeved at least once immediatelly,
   *                        even if it does not change immediatelly.
   * @see removeSVarUpdateFunctions
   */
  def addSVarUpdateFunctions[T : ClassTag : DataTag](svar: StateParticle[T], consumeSVar: Option[T => Unit],
                                                     getValueForSVar: Option[() => T] = None, useGet: Boolean = false, convertibleTrait: Option[ConvertibleTrait[T]] = None)  {
    if(!svar.isMutable && useGet) {
      get(Now, GetClosest, svar)( newValue => consumeSVar.collect{ case f => f(newValue._1) } )
      return
    }
    if(updateFunctionMap.contains(svar)) {
      println("[warn][SVarUpdateFunctionMap] You tried to call addSVarUpdateFunctions passing a svar ("+svar+") that has already been registered before for type " + convertibleTrait + ".")
      return
    }
    val changeableConsume = new ChangeableConsume(consumeSVar)
    svar.observe({(newValue: T) => changeableConsume.func.collect{case f => f(newValue)}}  , ignoredWriters)
    //if(useGet) get(svar)( (newValue: T) => {changeableConsume.func.collect{case f => f(newValue)}} )

    val updateSVar = getValueForSVar collect { case func => () => svar.set( func.apply(), Now ) }
    updateFunctionMap += svar -> GetAndSet (changeableConsume, updateSVar)
  }

  /**
   *  Adds two update functions to update an SVar from the value of its associated internal entity and vice versa.
   *        If there where already functions registered for this svar, the method will return and print a warning.
   * @param svar            An SVar
   * @param consumeSVar     A function, that updates the internal entity associated with svar.
   * @param getValueForSVar A function that retrieves the corresponding value from the internal entity associated with svar.
   * @see removeSVarUpdateFunctions
   */
  def addSVarUpdateFunctions[T : ClassTag : DataTag](svar: StateParticle[T], consumeSVar: T => Unit, getValueForSVar: => T) {
    addSVarUpdateFunctions(svar, Some(consumeSVar), Some(() => getValueForSVar))
  }

  /**
   *  Removes the update functions for this svar.
   *        After this it is possible to call addSVarUpdateFunctions for this svar anew.
   */
  def removeSVarUpdateFunctions(svar: StateParticle[_]) {
    svar.ignore()
    updateFunctionMap -= svar
  }

  /**
   *  Calles all updatedSvar functions that where previously added using addSVarUpdateFunctions.
   * @see addSVarUpdateFunctions
   */
  def updateAllSVars() {
    updateFunctionMap.foreach{ _._2.updateSVar.collect{case func => func.apply()}  }
  }

  /**
   *    Pauses the update mechanism for this svar.
   * This saves the registered update functions, removes them from the updateFunctionMap and
   *          calls sVar.ignore.
   *          The update mechanism cam be resumed later on.
   */
  def pauseUpdatesFor(svar: StateParticle[_]) {
    updateFunctionMap.get(svar).collect{
      case gAs =>
        updateFunctionMap -= svar
        pausedFunctionMap += svar -> gAs
        svar.ignore()
    }
  }


  /**
   *    Resumes the update mechanism for this svar.
   * This uses the stored update functions to resume updating
   *          just like before pauseUpdatesFor was called.
   * @param useGet If true, additionally to observe, get is called once on svar using consumeSVar.
   *               This way the svars current value guaranteed to be retreeved at least once immediatelly,
   *               even if it does not change immediatelly.
   * @see addSVarUpdateFunctions
   */
  def resumeUpdatesFor[T](svar: StateParticle[T], useGet: Boolean = false){
    _resumeUpdatesFor(svar, useGet)(svar.containedValueManifest.asInstanceOf[ClassTag[T]])
  }

  def _resumeUpdatesFor[T](svar: StateParticle[T], useGet: Boolean = false)(implicit typeTag : ClassTag[T]) {
    pausedFunctionMap.get(svar).collect{
      case gAs =>
        pausedFunctionMap -= svar
        updateFunctionMap += svar -> gAs
        gAs.asInstanceOf[GetAndSet[T]].consumeSVar.func.collect{case func =>
          svar.observe(func, ignoredWriters)
          if(useGet) svar get func
        }
    }
  }




  /**
   *  Changes the function that updates the internal rep when the svar has changed
   */
  private def updateConsumeSVar[T](svar: StateParticle[T], newConsumeSVar: Option[T => Unit]) {
    updateFunctionMap.get(svar).collect{
      case gAs => gAs.asInstanceOf[GetAndSet[T]].consumeSVar.func = newConsumeSVar
    }
  }

  /**
   *  Changes the function that updates the internal rep when the svar has changed
   */
  def updateConsumeSVar[T](svar: StateParticle[T], newConsumeSVar: T => Unit) {
    if(!svar.isMutable)
      println("[warn][SVarUpdateFunctionMap] You tried to call updateConsumeSVar passing an SVal ("+svar+").")
    updateConsumeSVar(svar, Some(newConsumeSVar))
  }

  /**
   *  Changes the function that updates the internal rep when the svar has changed
   *        to do nothing
   */
  def disableConsumeSvar[T](svar: SVar[T]) {
    updateConsumeSVar(svar, None)
  }

  /**
   *  Changes the function that updates the svar from the internal rep
   */
  private def updateGetValueForSVar[T](svar: StateParticle[T], newGetValueForSVar: Option[() => T]) {
    updateFunctionMap.get(svar).collect {
      case gAs => gAs.asInstanceOf[GetAndSet[T]].updateSVar = newGetValueForSVar match {
        case Some(func) => Some(() => {svar.set(  func.apply(), Now )})
        case None => None
      }
    }
  }


  /**
   *  Changes the function that updates the svar from the internal rep
   */
  def updateGetValueForSVar[T](svar: StateParticle[T], newGetValueForSVar: () => T) {
    if(!svar.isMutable)
      println("[warn][SVarUpdateFunctionMap] You tried to call updateConsumeSVar passing an immutable svar ("+svar+").")
    updateGetValueForSVar(svar, Some(newGetValueForSVar))
  }

  /**
   *  Disables the function that updates the svar from the internal rep
   */
  def disableGetValueForSVar[T](svar: SVar[T]) {
    updateGetValueForSVar(svar, None)
  }

  /**
   *  Changes the function that updates the internal rep when the svar has changed and
   *        the function that updates the svar from the internal rep at once.
   * @see   updateConsumeSVar, updateGetValueForSVar
   */
  def updateSVarUpdateFunctions[T](svar: StateParticle[T], newConsumeSVar: T => Unit, newGetValueForSVar: () => T) {
    updateConsumeSVar(svar, newConsumeSVar)
    updateGetValueForSVar(svar, newGetValueForSVar)
  }

  protected var multiobservers = Set[MultiObserve[_]]()

  protected case class MultiObserve[T](svar : StateParticle[T]){
    protected var handlers : Map[java.util.UUID, T => Any] = Map()
    svar.observe({ newValue => handlers.values.foreach(_.apply(newValue))}, ignoredWriters)

    def addHandler(handler : T => Any, id : java.util.UUID = java.util.UUID.randomUUID()){
      handlers = handlers.updated(id, handler)
    }
  }

  protected def getMultiObserve[T]( svar : StateParticle[T]) : MultiObserve[T] = multiobservers.find(_.svar equals svar) match {
    case Some(m : MultiObserve[T@unchecked]) => m
    case _ =>
      val retVal = MultiObserve(svar)
      multiobservers = multiobservers + retVal
      retVal
  }

  protected def addMultiobserve[T]( svar : StateParticle[T], useGet : Boolean = true)( handler : T => Unit){
    val observer   = getMultiObserve(svar)
    observer.addHandler(handler)
    if (useGet)
      svar.get(handler)
  }

  protected def ignoreMultiobserve( svar : StateParticle[_] ){
    multiobservers = multiobservers.filterNot(_.svar equals svar)
    svar.ignore()
  }
}
