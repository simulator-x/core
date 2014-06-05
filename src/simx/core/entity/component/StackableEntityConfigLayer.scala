package simx.core.entity.component

import simx.core.entity.description.{SValSet, EntityAspect}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.Entity

/**
 * User: dwiebusch
 * Date: 19.11.13
 * Time: 08:56
 */
trait StackableEntityConfigLayer extends EntityConfigLayer{
  private var rivImpl : PartialFunction[(Set[ConvertibleTrait[_]], EntityAspect, Entity, SValSet), Unit] = {
    case (_, a , _, _) => throw new Exception("providing initial values for aspect "+ a +" is not supported by " + this)
  }

  final protected def requestInitialValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity, given: SValSet){
    rivImpl.apply((toProvide, aspect, e, given))
  }

  final protected def provideInitialValuesFor(riv : PartialFunction[(Set[ConvertibleTrait[_]], EntityAspect, Entity, SValSet), Unit]){
    rivImpl = riv.orElse(rivImpl)
  }
}
