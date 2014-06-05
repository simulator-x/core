package simx.core.ontology.entities

import simx.core.ontology
import simx.core.entity.description.EntityAspect
import simx.core.ontology.SpecificDescription

case class ArmEntityDescription( asps : EntityAspect* ) extends SpecificDescription(ontology.types.Arm, asps.toList, simx.core.ontology.types.Acceleration, simx.core.ontology.types.Mass, simx.core.ontology.types.Transformation, simx.core.ontology.types.Alignment, simx.core.ontology.types.Velocity)

case class BarrelEntityDescription( asps : EntityAspect* ) extends SpecificDescription(ontology.types.Barrel, asps.toList)

case class GhostEntityDescription( asps : EntityAspect* ) extends SpecificDescription(ontology.types.Ghost, asps.toList, simx.core.ontology.types.Acceleration, simx.core.ontology.types.Transformation, simx.core.ontology.types.Mass, simx.core.ontology.types.Velocity)

case class HandEntityDescription( asps : EntityAspect* ) extends SpecificDescription(ontology.types.Hand, asps.toList, simx.core.ontology.types.Acceleration, simx.core.ontology.types.Mass, simx.core.ontology.types.Transformation, simx.core.ontology.types.Alignment, simx.core.ontology.types.Velocity)
case class HeadEntityDescription( asps : EntityAspect* ) extends SpecificDescription(ontology.types.Head, asps.toList, simx.core.ontology.types.Acceleration, simx.core.ontology.types.Transformation, simx.core.ontology.types.Mass, simx.core.ontology.types.Velocity)

case class SoundEntityDescription( asps : EntityAspect* ) extends SpecificDescription(ontology.types.Sound, asps.toList)

case class UserEntityDescription( asps : EntityAspect* ) extends SpecificDescription(ontology.types.User, asps.toList, simx.core.ontology.types.Acceleration, simx.core.ontology.types.Transformation, simx.core.ontology.types.Mass, simx.core.ontology.types.Velocity)

case class WellEntityDescription( asps : EntityAspect* ) extends SpecificDescription(ontology.types.Well, asps.toList, simx.core.ontology.types.Acceleration, simx.core.ontology.types.Transformation, simx.core.ontology.types.Mass, simx.core.ontology.types.Velocity)