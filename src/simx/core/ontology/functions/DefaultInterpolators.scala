/*
 * Copyright 2015 The SIRIS Project
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

package simx.core.ontology.functions

import java.awt.{Font, Color}
import java.awt.image.BufferedImage

import simplex3d.math.ConstVec2i
import simplex3d.math.float.{ConstVec4, Vec2, ConstVec2}
import simplex3d.math.floatx.{ConstQuat4f, ConstMat4f, ConstVec2f, ConstVec3f}
import simx.core.components.renderer.messages.EffectsConfiguration
import simx.core.components.renderer.setup.DisplaySetupDesc
import simx.core.entity.Entity
import simx.core.entity.description.{LinearInterpolator, SValSet, NamedSValSet, Interpolator}
import simx.core.helper.TextureData
import simx.core.helper.chirality.Chirality
import simx.core.ontology.GroundedSymbol
import simx.core.ontology.types.SoundProperties
import simx.core.ontology.types.TypeDefinitions.Enum
import simx.core.svaractor.SVarActor.Ref
import simx.core.svaractor.semantictrait.base.Thing
import simx.core.svaractor.unifiedaccess.Relation
import simx.core.worldinterface.eventhandling.EventDescription
import simplex3d.math.float.functions.slerp

import scala.collection.immutable.HashMap

class DataTypeBasedLinearInterpolator[DataType, S <: Thing] extends LinearInterpolator[DataType, S] {
  /**
   * Interpolates linearly between newer and older, based on ratio. The following boundary conditions have to be met:
   * Ratio = 0 has result in newer
   * Ratio = 1 has result in older
   */
  override def interpolate(newer: DataType, older: DataType, ratio: Float): DataType = {
    newer match{
      case newerValue: ConstVec3f =>
        val olderValue = older.asInstanceOf[ConstVec3f]
        ConstVec3f(newerValue + ((olderValue - newerValue) * ratio)).asInstanceOf[DataType]
      case newerValue: ConstVec2f =>
        val olderValue = older.asInstanceOf[ConstVec2f]
        ConstVec2(newerValue + ((olderValue - newerValue) * ratio)).asInstanceOf[DataType]
      case newerValue: Float =>
        val olderValue = older.asInstanceOf[Float]
        (newerValue + ((olderValue - newerValue) * ratio)).asInstanceOf[DataType]
      case newerValue: Int =>
        val olderValue = older.asInstanceOf[Int]
        (newerValue + ((olderValue - newerValue).toFloat * ratio)).toInt.asInstanceOf[DataType]
      case newerValue: ConstQuat4f =>
        val olderValue = older.asInstanceOf[ConstQuat4f]
        ConstQuat4f(slerp(newerValue, olderValue, ratio)).asInstanceOf[DataType]
      case newerValue: java.lang.Boolean =>
        val olderValue = older.asInstanceOf[java.lang.Boolean]
        (if(ratio <= 0.5f) newerValue else olderValue).asInstanceOf[DataType]
      case newerValue: simx.core.helper.Ray =>
        val olderValue = older.asInstanceOf[simx.core.helper.Ray]
        simx.core.helper.Ray(
          ConstVec3f(newerValue.origin    + ((olderValue.origin    - newerValue.origin)    * ratio)),
          ConstVec3f(newerValue.direction + ((olderValue.direction - newerValue.direction) * ratio))
        ).asInstanceOf[DataType]
      case _ => throw new Exception("[error][DataTypeBasedLinearInterpolator] No interpolation rule for type '" + newer.getClass.getCanonicalName + "' found.")
    }    
  }
}

/**
 * Created by martin 
 * on 01/06/15.
 */
object DefaultInterpolators extends Interpolators {
  override implicit val accelerationfactorInterpolator: Interpolator[Float,simx.core.ontology.Symbols.accelerationFactor.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val realInterpolator: Interpolator[Float,simx.core.ontology.Symbols.real.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val configurationInterpolator: Interpolator[NamedSValSet,simx.core.ontology.Symbols.configuration.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val righttextureInterpolator: Interpolator[String,simx.core.ontology.Symbols.rightTexture.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_homeInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_Home.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val tuiocursorsInterpolator: Interpolator[HashMap[Symbol, ConstVec2],simx.core.ontology.Symbols.tuioCursors.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val tangiblesurfaceInterpolator: Interpolator[Entity,simx.core.ontology.Symbols.tangibleSurface.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val meshvertexpositionsInterpolator: Interpolator[Array[Float],simx.core.ontology.Symbols.meshVertexPositions.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val intensityInterpolator: Interpolator[Float,simx.core.ontology.Symbols.intensity.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_uInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_u.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val componentInterpolator: Interpolator[Ref,simx.core.ontology.Symbols.component.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val integerInterpolator: Interpolator[Int,simx.core.ontology.Symbols.integer.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val accelerationInterpolator: Interpolator[ConstVec3f,simx.core.ontology.Symbols.acceleration.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val massInterpolator: Interpolator[Float,simx.core.ontology.Symbols.mass.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_tInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_t.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val imageInterpolator: Interpolator[BufferedImage,simx.core.ontology.Symbols.image.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_fInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_f.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_2Interpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_2.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val button_centerInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.button_Center.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val linearattenuationInterpolator: Interpolator[Float,simx.core.ontology.Symbols.linearAttenuation.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val diffusecolorInterpolator: Interpolator[Color,simx.core.ontology.Symbols.diffuseColor.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_plusInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_Plus.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val radiusInterpolator: Interpolator[Float,simx.core.ontology.Symbols.radius.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val entityInterpolator: Interpolator[Entity,simx.core.ontology.Symbols.entity.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val sizeInterpolator: Interpolator[Float,simx.core.ontology.Symbols.size.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val meshindicesInterpolator: Interpolator[Array[Int],simx.core.ontology.Symbols.meshIndices.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val materialInterpolator: Interpolator[String,simx.core.ontology.Symbols.material.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_mInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_m.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_9Interpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_9.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val containerInterpolator: Interpolator[SValSet,simx.core.ontology.Symbols.container.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_1Interpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_1.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val meshnormalsInterpolator: Interpolator[Array[Float],simx.core.ontology.Symbols.meshNormals.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val matrixInterpolator: Interpolator[ConstMat4f,simx.core.ontology.Symbols.matrix.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_upInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_Up.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val uptextureInterpolator: Interpolator[String,simx.core.ontology.Symbols.upTexture.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_lInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_l.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val scaleInterpolator: Interpolator[ConstMat4f,simx.core.ontology.Symbols.scale.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val eventdescriptionInterpolator: Interpolator[EventDescription,simx.core.ontology.Symbols.eventDescription.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_eInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_e.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val enabledInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.enabled.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val booleanInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.boolean.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val componenttypeInterpolator: Interpolator[GroundedSymbol,simx.core.ontology.Symbols.componentType.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val effectsconfigurationInterpolator: Interpolator[EffectsConfiguration,simx.core.ontology.Symbols.effectsConfiguration.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val jvrInterpolator: Interpolator[Ref,simx.core.ontology.Symbols.jVR.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_dInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_d.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val meshInterpolator: Interpolator[Any,simx.core.ontology.Symbols.mesh.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val tuioobjectsInterpolator: Interpolator[Map[Symbol, (ConstVec2, Float)],simx.core.ontology.Symbols.tuioObjects.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val attractorstrengthInterpolator: Interpolator[Int,simx.core.ontology.Symbols.attractorStrength.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val lengthInterpolator: Interpolator[Float,simx.core.ontology.Symbols.length.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val identifiersInterpolator: Interpolator[List[Symbol],simx.core.ontology.Symbols.identifiers.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val viewplatformInterpolator: Interpolator[ConstMat4f,simx.core.ontology.Symbols.viewPlatform.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val fontInterpolator: Interpolator[Font,simx.core.ontology.Symbols.font.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val actorInterpolator: Interpolator[Ref,simx.core.ontology.Symbols.actor.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val relationInterpolator: Interpolator[Relation,simx.core.ontology.Symbols.relation.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val directionInterpolator: Interpolator[ConstVec3f,simx.core.ontology.Symbols.direction.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val skyboxfogInterpolator: Interpolator[Float,simx.core.ontology.Symbols.skyBoxFog.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val colladaobjectidInterpolator: Interpolator[String,simx.core.ontology.Symbols.colladaObjectId.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val thresholdInterpolator: Interpolator[Float,simx.core.ontology.Symbols.threshold.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val saturationInterpolator: Interpolator[Float,simx.core.ontology.Symbols.saturation.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_zInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_z.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_8Interpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_8.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val timeinsecondsInterpolator: Interpolator[Float,simx.core.ontology.Symbols.timeInSeconds.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val resolutionInterpolator: Interpolator[ConstVec2i,simx.core.ontology.Symbols.resolution.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val longInterpolator: Interpolator[Long,simx.core.ontology.Symbols.long.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_sInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_s.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val vector4Interpolator: Interpolator[ConstVec4,simx.core.ontology.Symbols.vector4.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val subelementInterpolator: Interpolator[String,simx.core.ontology.Symbols.subElement.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_yInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_y.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val chiralityInterpolator: Interpolator[Chirality,simx.core.ontology.Symbols.chirality.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val stringInterpolator: Interpolator[String,simx.core.ontology.Symbols.string.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_7Interpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_7.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val impulseInterpolator: Interpolator[ConstVec3f,simx.core.ontology.Symbols.impulse.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val vector3Interpolator: Interpolator[ConstVec3f,simx.core.ontology.Symbols.vector3.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val minattractordistanceInterpolator: Interpolator[Float,simx.core.ontology.Symbols.minAttractorDistance.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val keyInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val parentelementInterpolator: Interpolator[Entity,simx.core.ontology.Symbols.parentElement.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_0Interpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_0.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val drawlinesInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.drawLines.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val textureInterpolator: Interpolator[TextureData,simx.core.ontology.Symbols.texture.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_kInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_k.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val ipInterpolator: Interpolator[String,simx.core.ontology.Symbols.ip.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_rInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_r.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val buttonInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.button.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val frameInterpolator: Interpolator[BufferedImage,simx.core.ontology.Symbols.frame.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val factorInterpolator: Interpolator[Float,simx.core.ontology.Symbols.factor.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val anylistInterpolator: Interpolator[List[Nothing],simx.core.ontology.Symbols.anyList.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val vector4listInterpolator: Interpolator[List[ConstVec4],simx.core.ontology.Symbols.vector4List.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_6Interpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_6.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val upvectorInterpolator: Interpolator[ConstVec3f,simx.core.ontology.Symbols.upVector.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_jInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_j.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val healthInterpolator: Interpolator[Float,simx.core.ontology.Symbols.health.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val manaInterpolator: Interpolator[Float,simx.core.ontology.Symbols.mana.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val thicknessInterpolator: Interpolator[Float,simx.core.ontology.Symbols.thickness.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val colorInterpolator: Interpolator[Color,simx.core.ontology.Symbols.color.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_qInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_q.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val manipulatorlistInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.manipulatorList.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val transformationInterpolator: Interpolator[ConstMat4f,simx.core.ontology.Symbols.transformation.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_cInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_c.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val angularfactorInterpolator: Interpolator[Float,simx.core.ontology.Symbols.angularFactor.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val simulationspeedInterpolator: Interpolator[Float,simx.core.ontology.Symbols.simulationSpeed.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val wavescaleInterpolator: Interpolator[Float,simx.core.ontology.Symbols.waveScale.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_iInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_i.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val attenuationInterpolator: Interpolator[Float,simx.core.ontology.Symbols.attenuation.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val mindistancetoneighborsInterpolator: Interpolator[Float,simx.core.ontology.Symbols.minDistanceToNeighbors.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val stateInterpolator: Interpolator[String,simx.core.ontology.Symbols.state.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val angulardampingInterpolator: Interpolator[Float,simx.core.ontology.Symbols.angularDamping.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val mouseInterpolator: Interpolator[Entity,simx.core.ontology.Symbols.mouse.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val entitytypeInterpolator: Interpolator[GroundedSymbol,simx.core.ontology.Symbols.entityType.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val componentnameInterpolator: Interpolator[String,simx.core.ontology.Symbols.componentName.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val maxneighborsInterpolator: Interpolator[Int,simx.core.ontology.Symbols.maxNeighbors.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val vector2listInterpolator: Interpolator[List[Vec2],simx.core.ontology.Symbols.vector2List.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val farclipInterpolator: Interpolator[Float,simx.core.ontology.Symbols.farClip.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_bInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_b.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val audiofileInterpolator: Interpolator[String,simx.core.ontology.Symbols.audioFile.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val castshadowInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.castShadow.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val probabilityInterpolator: Interpolator[Float,simx.core.ontology.Symbols.probability.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val orientationInterpolator: Interpolator[ConstQuat4f,simx.core.ontology.Symbols.orientation.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val maxaccmagInterpolator: Interpolator[Float,simx.core.ontology.Symbols.maxAccMag.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_leftInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_Left.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val lineardampingInterpolator: Interpolator[Float,simx.core.ontology.Symbols.linearDamping.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val enumerationInterpolator: Interpolator[Enum,simx.core.ontology.Symbols.enumeration.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val typInterpolator: Interpolator[GroundedSymbol,simx.core.ontology.Symbols.typ.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val heightInterpolator: Interpolator[Float,simx.core.ontology.Symbols.height.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val identifierInterpolator: Interpolator[Symbol,simx.core.ontology.Symbols.identifier.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val timeInterpolator: Interpolator[Long,simx.core.ontology.Symbols.time.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val fpsInterpolator: Interpolator[Float,simx.core.ontology.Symbols.fps.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val applicationInterpolator: Interpolator[String,simx.core.ontology.Symbols.application.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val keyboardInterpolator: Interpolator[Entity,simx.core.ontology.Symbols.keyboard.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val portInterpolator: Interpolator[Int,simx.core.ontology.Symbols.port.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val headtransformInterpolator: Interpolator[ConstMat4f,simx.core.ontology.Symbols.headTransform.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_xInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_x.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val discreteposition2dInterpolator: Interpolator[ConstVec2i,simx.core.ontology.Symbols.discretePosition2D.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val positionInterpolator: Interpolator[ConstVec3f,simx.core.ontology.Symbols.position.SymbolType] =
    new LinearInterpolator[ConstVec3f, simx.core.ontology.Symbols.position.SymbolType] {
      override def interpolate(a: ConstVec3f, b: ConstVec3f, ratio: Float): ConstVec3f = a + ((b - a) * ratio)
    }
  override implicit val interactionsInterpolator: Interpolator[List[Nothing],simx.core.ontology.Symbols.interactions.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val displaysetupdescriptionInterpolator: Interpolator[DisplaySetupDesc,simx.core.ontology.Symbols.displaySetupDescription.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val vector2Interpolator: Interpolator[ConstVec2,simx.core.ontology.Symbols.vector2.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val meshtexcoordsInterpolator: Interpolator[Array[Float],simx.core.ontology.Symbols.meshTexCoords.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val mindistancetopredatorsInterpolator: Interpolator[Float,simx.core.ontology.Symbols.minDistanceToPredators.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val shadowbiasInterpolator: Interpolator[Float,simx.core.ontology.Symbols.shadowBias.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val restitutionInterpolator: Interpolator[Float,simx.core.ontology.Symbols.restitution.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val realsInterpolator: Interpolator[Array[Float],simx.core.ontology.Symbols.reals.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_wInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_w.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val gravityInterpolator: Interpolator[ConstVec3f,simx.core.ontology.Symbols.gravity.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_5Interpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_5.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val openalpropsInterpolator: Interpolator[SoundProperties,simx.core.ontology.Symbols.openAlProps.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_pInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_p.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val initializeInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.initialize.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val relationsubjectInterpolator: Interpolator[Entity,simx.core.ontology.Symbols.relationSubject.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val namedcontainerInterpolator: Interpolator[NamedSValSet,simx.core.ontology.Symbols.namedContainer.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val button_rightInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.button_Right.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val lefttextureInterpolator: Interpolator[String,simx.core.ontology.Symbols.leftTexture.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val screencoordinatesInterpolator: Interpolator[List[ConstVec2],simx.core.ontology.Symbols.screenCoordinates.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val integersInterpolator: Interpolator[Array[Int],simx.core.ontology.Symbols.integers.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val originInterpolator: Interpolator[ConstVec3f,simx.core.ontology.Symbols.origin.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_hInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_h.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val alignmentInterpolator: Interpolator[Enum,simx.core.ontology.Symbols.alignment.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val fileInterpolator: Interpolator[String,simx.core.ontology.Symbols.file.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_spaceInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_Space.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val colladafileInterpolator: Interpolator[String,simx.core.ontology.Symbols.colladaFile.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val damageInterpolator: Interpolator[Float,simx.core.ontology.Symbols.damage.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val showheightmapInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.showHeightMap.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val velocityInterpolator: Interpolator[ConstVec3f,simx.core.ontology.Symbols.velocity.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val maxattractorsInterpolator: Interpolator[Int,simx.core.ontology.Symbols.maxAttractors.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_aInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_a.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_oInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_o.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_vInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_v.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val downtextureInterpolator: Interpolator[String,simx.core.ontology.Symbols.downTexture.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_4Interpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_4.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val quadraticattenuationInterpolator: Interpolator[Float,simx.core.ontology.Symbols.quadraticAttenuation.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val livesInterpolator: Interpolator[Int,simx.core.ontology.Symbols.lives.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val nearclipInterpolator: Interpolator[Float,simx.core.ontology.Symbols.nearClip.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val spotcutoffInterpolator: Interpolator[Float,simx.core.ontology.Symbols.spotCutOff.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val attractorattractionfactorInterpolator: Interpolator[Float,simx.core.ontology.Symbols.attractorAttractionFactor.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_downInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_Down.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val halfextendsInterpolator: Interpolator[ConstVec3f,simx.core.ontology.Symbols.halfExtends.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_nInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_n.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val backtextureInterpolator: Interpolator[String,simx.core.ontology.Symbols.backTexture.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_rightInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_Right.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val normalInterpolator: Interpolator[ConstVec3f,simx.core.ontology.Symbols.normal.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_gInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_g.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val spotexponentInterpolator: Interpolator[Float,simx.core.ontology.Symbols.spotExponent.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val angleInterpolator: Interpolator[Float,simx.core.ontology.Symbols.angle.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val bytesInterpolator: Interpolator[Array[Byte],simx.core.ontology.Symbols.bytes.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val button_leftInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.button_Left.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val relationobjectInterpolator: Interpolator[Entity,simx.core.ontology.Symbols.relationObject.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_3Interpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_3.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val neighborInterpolator: Interpolator[Entity,simx.core.ontology.Symbols.neighbor.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val key_minusInterpolator: Interpolator[Boolean,simx.core.ontology.Symbols.key_Minus.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val constantattenuationInterpolator: Interpolator[Float,simx.core.ontology.Symbols.constantAttenuation.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val position2dInterpolator: Interpolator[ConstVec2f,simx.core.ontology.Symbols.position2D.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val nameInterpolator: Interpolator[String,simx.core.ontology.Symbols.name.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val specularcolorInterpolator: Interpolator[Color,simx.core.ontology.Symbols.specularColor.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val fronttextureInterpolator: Interpolator[String,simx.core.ontology.Symbols.frontTexture.SymbolType] = new DataTypeBasedLinearInterpolator
  override implicit val byteInterpolator: Interpolator[Byte,simx.core.ontology.Symbols.byte.SymbolType] = new DataTypeBasedLinearInterpolator
}
