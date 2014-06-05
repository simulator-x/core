/*
  * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package simx.core.helper

import scala.collection._
import scala.reflect.runtime.universe.TypeTag
import simx.core.entity.Entity
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.description.{SVal, SValSet}
import simx.core.svaractor.{SVarActor, SVar}
import scala.reflect.ClassTag

/**
 *  A trait for updating component-internal entities and their associated svars using a hashmap.
 */
trait SVarUpdateFunctionMap extends SVarActor {

  /**
   *  Stores a consume function for a svar.
   */
  protected class ChangeableConsume[T](var func: Option[T => Unit])

  /**
   *  Stores functions to update an SVar from the value of its associated internal entity and vice versa.
   * @see addSVarUpdateFunctions
   */
  protected case class GetAndSet[T](consumeSVar:  ChangeableConsume[T], var updateSVar: Option[() => Unit])

  private val updateFunctionMap = mutable.Map[SVar[_], GetAndSet[_]]()
  private val pausedFunctionMap = mutable.Map[SVar[_], GetAndSet[_]]()

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
    val targetSize = (for(ct <- toGet.toSeq) yield e.get(ct).size).foldLeft(0)(_+_)
    var retrievedValues = Map[SVar[_], SVal[_]]()//new SValSet()

    toGet.foreach((ct: ConvertibleTrait[_]) => {init(ct)})

    def init[T : ClassTag](ct: ConvertibleTrait[T]) {
      e.get(ct).foreach(sVar => {addSVarUpdateFunctions(sVar, Some(valueArrived[T](ct, sVar) _), None, useGet = true)})
    }

    def clean[T](ct: ConvertibleTrait[T]) {
      e.get(ct).foreach(sVar => updateConsumeSVar(sVar, None))
      e.get(ct).foreach(sVar => updateGetValueForSVar(sVar, None))
    }

    def remove[T](ct: ConvertibleTrait[T]) {
      e.get(ct).foreach(sVar => removeSVarUpdateFunctions(sVar))
    }

    def valueArrived[T](ct: ConvertibleTrait[T], sVar: SVar[T])(value: T) {
      retrievedValues = retrievedValues.updated(sVar, SVal(ct)(value))
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
  def addSVarUpdateFunctions[T : ClassTag](svar: SVar[T], consumeSVar: Option[T => Unit],
                                getValueForSVar: Option[() => T] = None, useGet: Boolean = false)  {
    if(updateFunctionMap.contains(svar)) {
      println("[warn][SVarUpdateFunctionMap] You tried to call addSVarUpdateFunctions passing a svar that has already been registered before.")
      return
    }
    val changeableConsume = new ChangeableConsume(consumeSVar)
    observe( svar, ignoredWriters )((newValue: T) => {changeableConsume.func.collect{case f => f(newValue)}} )
    if(useGet) get( svar)( (newValue: T) => {changeableConsume.func.collect{case f => f(newValue)}} )

    val updateSVar = getValueForSVar collect { case func => (() => set( svar, func.apply() )) }
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
  def addSVarUpdateFunctions[T : ClassTag](svar: SVar[T], consumeSVar: T => Unit, getValueForSVar: => T) {
    addSVarUpdateFunctions(svar, Some(consumeSVar), Some(() => getValueForSVar))
  }

  /**
   *  Removes the update functions for this svar.
   *        After this it is possible to call addSVarUpdateFunctions for this svar anew.
   */
  def removeSVarUpdateFunctions(svar: SVar[_]) {
    ignore( svar )
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
  def pauseUpdatesFor(svar: SVar[_]) {
    updateFunctionMap.get(svar).collect{
      case gAs =>
        updateFunctionMap -= svar
        pausedFunctionMap += svar -> gAs
        ignore( svar )
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
  def resumeUpdatesFor[T](svar: SVar[T], useGet: Boolean = false){
    _resumeUpdatesFor(svar, useGet)(svar.containedValueManifest)
  }

  def _resumeUpdatesFor[T](svar: SVar[T], useGet: Boolean = false)(implicit typeTag : ClassTag[T]) {
    pausedFunctionMap.get(svar).collect{
      case gAs =>
        pausedFunctionMap -= svar
        updateFunctionMap += svar -> gAs
        gAs.asInstanceOf[GetAndSet[T]].consumeSVar.func.collect{case func =>
          observe( svar, ignoredWriters)( func )
          if(useGet) get( svar)( func )
        }
    }
  }




  /**
   *  Changes the function that updates the internal rep when the svar has changed
   */
  private def updateConsumeSVar[T](svar: SVar[T], newConsumeSVar: Option[T => Unit]) {
    updateFunctionMap.get(svar).collect{
      case gAs => gAs.asInstanceOf[GetAndSet[T]].consumeSVar.func = newConsumeSVar
    }
  }

  /**
   *  Changes the function that updates the internal rep when the svar has changed
   */
  def updateConsumeSVar[T](svar: SVar[T], newConsumeSVar: T => Unit) {
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
  private def updateGetValueForSVar[T](svar: SVar[T], newGetValueForSVar: Option[() => T]) {
    updateFunctionMap.get(svar).collect {
      case gAs => gAs.asInstanceOf[GetAndSet[T]].updateSVar = newGetValueForSVar match {
        case Some(func) => Some(() => {set( svar, func.apply() )})
        case None => None
      }
    }
  }


  /**
   *  Changes the function that updates the svar from the internal rep
   */
  def updateGetValueForSVar[T](svar: SVar[T], newGetValueForSVar: () => T) {
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
  def updateSVarUpdateFunctions[T](svar: SVar[T], newConsumeSVar: T => Unit, newGetValueForSVar: () => T) {
    updateConsumeSVar(svar, newConsumeSVar)
    updateGetValueForSVar(svar, newGetValueForSVar)
  }

  protected var multiobservers = Set[MultiObserve[_]]()

  protected case class MultiObserve[T](svar : SVar[T]){
    protected var handlers : Map[java.util.UUID, T => Any] = Map()
    observe(svar, ignoredWriters){ newValue => handlers.values.foreach(_.apply(newValue))}

    def addHandler(handler : T => Any, id : java.util.UUID = java.util.UUID.randomUUID()){
      handlers = handlers.updated(id, handler)
    }
  }

  protected def getMultiObserve[T]( svar : SVar[T]) : MultiObserve[T] = multiobservers.find(_.svar equals svar) match {
    case Some(m : MultiObserve[T]) => m
    case _ =>
      val retVal = MultiObserve(svar)
      multiobservers = multiobservers + retVal
      retVal
  }

  protected def addMultiobserve[T]( svar : SVar[T], useGet : Boolean = true)( handler : T => Any){
    val observer   = getMultiObserve(svar)
    observer.addHandler(handler)
    if (useGet)
      svar.get(handler)
  }

  protected def ignoreMultiobserve[T]( svar : SVar[T] ){
    multiobservers = multiobservers.filterNot(_.svar equals svar)
    svar.ignore()
  }
}
