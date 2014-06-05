package simx.core.ontology 

/*
 * Copyright 2013 The SIRIS Project
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

 import simx.core.ontology.entities._
 import simx.core.ontology.{Symbols, SVarDescription}

 package object types {
   def init(){}
                            object Acceleration extends SVarDescription[simplex3d.math.floatx.ConstVec3f, simplex3d.math.floatx.ConstVec3f](simx.core.ontology.types.Vector3 as Symbols.acceleration definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#Acceleration")
object AccelerationFactor extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.accelerationFactor definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#AccelerationFactor")
object Alignment extends SVarDescription[simx.core.ontology.types.TypeDefinitions.Enum, simx.core.ontology.types.TypeDefinitions.Enum](simx.core.ontology.types.Enumeration as Symbols.alignment definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#Alignment")
object Angle extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.angle definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Angle")
object AngularDamping extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.angularDamping definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#AngularDamping")
object AngularFactor extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.angularFactor definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#AngularFactor")
object AnyList extends SVarDescription[scala.collection.immutable.List[Nothing], scala.collection.immutable.List[Nothing]](simx.core.ontology.types.NullType as Symbols.anyList createdBy List[Nothing]() definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#AnyList")
object Arm extends EntitySVarDescription[simx.core.ontology.entities.Arm](Symbols.arm, new Arm(_) )
object Attenuation extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.attenuation definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Attenuation")
object AttractorAttractionFactor extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.attractorAttractionFactor definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#AttractorAttractionFactor")
object AttractorStrength extends SVarDescription[scala.Int, scala.Int](simx.core.ontology.types.Integer as Symbols.attractorStrength definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#AttractorStrength")
object AudioFile extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.File as Symbols.audioFile definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#AudioFile")

object BackTexture extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.String as Symbols.backTexture definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#BackTexture")
object Barrel extends EntitySVarDescription[simx.core.ontology.entities.Barrel](Symbols.barrel, new Barrel(_) )
object Boolean extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.NullType as Symbols.boolean createdBy false definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Boolean")
object Button extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.button definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Button")
object Button_Center extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Button as Symbols.button_Center definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Button_Center")
object Button_Left extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Button as Symbols.button_Left definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Button_Left")
object Button_Right extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Button as Symbols.button_Right definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Button_Right")
object Byte extends SVarDescription[scala.Byte, scala.Byte](simx.core.ontology.types.NullType as Symbols.byte createdBy 0.toByte definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Byte")
object Bytes extends SVarDescription[scala.Array[scala.Byte], scala.Array[scala.Byte]](simx.core.ontology.types.NullType as Symbols.bytes createdBy (Array[Byte]() ) definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Bytes")

object CastShadow extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.castShadow definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#CastShadow")
object ColladaFile extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.File as Symbols.colladaFile definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#ColladaFile")
object ColladaObjectId extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.String as Symbols.colladaObjectId definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#ColladaObjectId")
object Color extends SVarDescription[java.awt.Color, java.awt.Color](simx.core.ontology.types.NullType as Symbols.color createdBy (java.awt.Color.WHITE) definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Color")
object ComputerGraphics extends SVarDescription[simplex3d.math.floatx.ConstMat4f, simplex3d.math.floatx.ConstMat4f](simx.core.ontology.types.Transformation as Symbols.computerGraphics definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#ComputerGraphics")
object Configuration extends SVarDescription[simx.core.entity.description.NamedSValSet, simx.core.entity.description.NamedSValSet](simx.core.ontology.types.NamedContainer as Symbols.configuration definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Configuration")
object ConstantAttenuation extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.constantAttenuation definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#ConstantAttenuation")
object Container extends SVarDescription[simx.core.entity.description.SValSet, simx.core.entity.description.SValSet](simx.core.ontology.types.NullType as Symbols.container createdBy (new simx.core.entity.description.SValSet()) definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Container")

object Damage extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.damage definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Damage")
object DiffuseColor extends SVarDescription[java.awt.Color, java.awt.Color](simx.core.ontology.types.Color as Symbols.diffuseColor definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#DiffuseColor")
object Direction extends SVarDescription[simplex3d.math.floatx.ConstVec3f, simplex3d.math.floatx.ConstVec3f](simx.core.ontology.types.Vector3 as Symbols.direction definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Direction")
object DownTexture extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.String as Symbols.downTexture definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#DownTexture")
object DrawLines extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.drawLines definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#DrawLines")

object Enabled extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.enabled definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Enabled")
object Entity extends SVarDescription[simx.core.entity.Entity, simx.core.entity.Entity](simx.core.ontology.types.NullType as Symbols.entity createdBy new simx.core.entity.Entity() definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#Entity")
object Enumeration extends SVarDescription[simx.core.ontology.types.TypeDefinitions.Enum, simx.core.ontology.types.TypeDefinitions.Enum](simx.core.ontology.types.NullType as Symbols.enumeration createdBy (simx.core.ontology.types.DefaultEnum.Foo) definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Enumeration")
object EventDescription extends SVarDescription[simx.core.worldinterface.eventhandling.EventDescription, simx.core.worldinterface.eventhandling.EventDescription](simx.core.ontology.types.NullType as Symbols.eventDescription createdBy (new simx.core.worldinterface.eventhandling.EventDescription(simx.core.ontology.Symbols.nullType)) definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#EventDescription")

object Factor extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.factor definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Factor")
object FarClip extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.farClip definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#FarClip")
object File extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.NullType as Symbols.file createdBy new String("") definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#File")
object Frame extends SVarDescription[java.awt.image.BufferedImage, java.awt.image.BufferedImage](simx.core.ontology.types.NullType as Symbols.frame createdBy (new java.awt.image.BufferedImage(1,1,1)) definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Frame")
object FrontTexture extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.String as Symbols.frontTexture definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#FrontTexture")

object Ghost extends EntitySVarDescription[simx.core.ontology.entities.Ghost](Symbols.ghost, new Ghost(_) )
object Gravity extends SVarDescription[simplex3d.math.floatx.ConstVec3f, simplex3d.math.floatx.ConstVec3f](simx.core.ontology.types.Vector3 as Symbols.gravity definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Gravity")

object HalfExtends extends SVarDescription[simplex3d.math.floatx.ConstVec3f, simplex3d.math.floatx.ConstVec3f](simx.core.ontology.types.Vector3 as Symbols.halfExtends definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#HalfExtends")
object Hand extends EntitySVarDescription[simx.core.ontology.entities.Hand](Symbols.hand, new Hand(_) )
object Head extends EntitySVarDescription[simx.core.ontology.entities.Head](Symbols.head, new Head(_) )
object HeadTransform extends SVarDescription[simplex3d.math.floatx.ConstMat4f, simplex3d.math.floatx.ConstMat4f](simx.core.ontology.types.Transformation as Symbols.headTransform definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#HeadTransform")
object Health extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.health definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Health")
object Height extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.height definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Height")

object Identifier extends SVarDescription[scala.Symbol, scala.Symbol](simx.core.ontology.types.NullType as Symbols.identifier createdBy Symbol("") definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Identifier")
object Impulse extends SVarDescription[simplex3d.math.floatx.ConstVec3f, simplex3d.math.floatx.ConstVec3f](simx.core.ontology.types.Vector3 as Symbols.impulse definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Impulse")
object Initialize extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.initialize definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Initialize")
object Integer extends SVarDescription[scala.Int, scala.Int](simx.core.ontology.types.NullType as Symbols.integer createdBy 0 definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Integer")
object Intensity extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.intensity definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Intensity")
object Interactions extends SVarDescription[scala.collection.immutable.List[Nothing], scala.collection.immutable.List[Nothing]](simx.core.ontology.types.AnyList as Symbols.interactions definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Interactions")
object Ip extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.String as Symbols.ip definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Ip")

object Key extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key")
object Key_0 extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_0 definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_0")
object Key_1 extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_1 definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_1")
object Key_2 extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_2 definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_2")
object Key_3 extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_3 definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_3")
object Key_4 extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_4 definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_4")
object Key_5 extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_5 definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_5")
object Key_6 extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_6 definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_6")
object Key_7 extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_7 definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_7")
object Key_8 extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_8 definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_8")
object Key_9 extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_9 definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_9")
object Key_Down extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_Down definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_Down")
object Key_Home extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_Home definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_Home")
object Key_Left extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_Left definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_Left")
object Key_Minus extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_Minus definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_Minus")
object Key_Plus extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_Plus definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_Plus")
object Key_Right extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_Right definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_Right")
object Key_Space extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_Space definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_Space")
object Key_Up extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_Up definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_Up")
object Key_a extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_a definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_a")
object Key_b extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_b definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_b")
object Key_c extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_c definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_c")
object Key_d extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_d definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_d")
object Key_e extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_e definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_e")
object Key_f extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_f definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_f")
object Key_g extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_g definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_g")
object Key_h extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_h definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_h")
object Key_i extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_i definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_i")
object Key_j extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_j definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_j")
object Key_k extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_k definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_k")
object Key_l extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_l definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_l")
object Key_m extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_m definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_m")
object Key_n extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_n definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_n")
object Key_o extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_o definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_o")
object Key_p extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_p definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_p")
object Key_q extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_q definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_q")
object Key_r extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_r definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_r")
object Key_s extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_s definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_s")
object Key_t extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_t definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_t")
object Key_u extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_u definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_u")
object Key_v extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_v definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_v")
object Key_w extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_w definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_w")
object Key_x extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_x definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_x")
object Key_y extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_y definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_y")
object Key_z extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.key_z definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Key_z")
object Keyboard extends SVarDescription[simx.core.entity.Entity, simx.core.entity.Entity](simx.core.ontology.types.Entity as Symbols.keyboard definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Keyboard")
 
object LeftTexture extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.String as Symbols.leftTexture definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#LeftTexture")
object LinearAttenuation extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.linearAttenuation definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#LinearAttenuation")
object LinearDamping extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.linearDamping definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#LinearDamping")
object Lives extends SVarDescription[scala.Int, scala.Int](simx.core.ontology.types.Integer as Symbols.lives definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Lives")
object Long extends SVarDescription[scala.Long, scala.Long](simx.core.ontology.types.NullType as Symbols.long createdBy 0L definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Long")

object Mana extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.mana definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Mana")
object ManipulatorList extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.manipulatorList definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#ManipulatorList")
object Mass extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.mass definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#Mass")
object Material extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.String as Symbols.material definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Material")
object Matrix extends SVarDescription[simplex3d.math.floatx.ConstMat4f, simplex3d.math.floatx.ConstMat4f](simx.core.ontology.types.NullType as Symbols.matrix createdBy simplex3d.math.floatx.Mat4f.Identity definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#Matrix")
object MaxAccMag extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.maxAccMag definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#MaxAccMag")
object MaxAttractors extends SVarDescription[scala.Int, scala.Int](simx.core.ontology.types.Integer as Symbols.maxAttractors definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#MaxAttractors")
object MaxNeighbors extends SVarDescription[scala.Int, scala.Int](simx.core.ontology.types.Integer as Symbols.maxNeighbors definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#MaxNeighbors")
object MinAttractorDistance extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.minAttractorDistance definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#MinAttractorDistance")
object MinDistanceToNeighbors extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.minDistanceToNeighbors definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#MinDistanceToNeighbors")
object MinDistanceToPredators extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.minDistanceToPredators definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#MinDistanceToPredators")
object Mouse extends SVarDescription[simx.core.entity.Entity, simx.core.entity.Entity](simx.core.ontology.types.Entity as Symbols.mouse definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Mouse")

object Name extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.String as Symbols.name definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Name")
object NamedContainer extends SVarDescription[simx.core.entity.description.NamedSValSet, simx.core.entity.description.NamedSValSet](simx.core.ontology.types.NullType as Symbols.namedContainer createdBy (new simx.core.entity.description.NamedSValSet(simx.core.ontology.Symbols.nullType)) definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#NamedContainer")
object NearClip extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.nearClip definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#NearClip")
object Neighbor extends SVarDescription[simx.core.entity.Entity, simx.core.entity.Entity](simx.core.ontology.types.Entity as Symbols.neighbor definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Neighbor")
object Normal extends SVarDescription[simplex3d.math.floatx.ConstVec3f, simplex3d.math.floatx.ConstVec3f](simx.core.ontology.types.Vector3 as Symbols.normal createdBy (simplex3d.math.float.ConstVec3(1,0,0)) definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Normal")

object OpenAlProps extends SVarDescription[simx.core.ontology.types.SoundProperties, simx.core.ontology.types.SoundProperties](simx.core.ontology.types.NullType as Symbols.openAlProps createdBy simx.core.ontology.types.SoundProperties() definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/components/sound/SimxSound.owl#OpenAlProps")
object Origin extends SVarDescription[simplex3d.math.floatx.ConstVec3f, simplex3d.math.floatx.ConstVec3f](simx.core.ontology.types.Vector3 as Symbols.origin definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Origin")

object ParentElement extends SVarDescription[simx.core.entity.Entity, simx.core.entity.Entity](simx.core.ontology.types.Entity as Symbols.parentElement definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#ParentElement")
object Port extends SVarDescription[scala.Int, scala.Int](simx.core.ontology.types.Integer as Symbols.port definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Port")
object Position extends SVarDescription[simplex3d.math.floatx.ConstVec3f, simplex3d.math.floatx.ConstVec3f](simx.core.ontology.types.Vector3 as Symbols.position definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Position")
object Position2D extends SVarDescription[simplex3d.math.floatx.ConstVec2f, simplex3d.math.floatx.ConstVec2f](simx.core.ontology.types.Vector2 as Symbols.position2D definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Position2D")
object Probability extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.probability definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Probability")

object QuadraticAttenuation extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.quadraticAttenuation definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#QuadraticAttenuation")

object Radius extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.radius definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Radius")
object Real extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.NullType as Symbols.real createdBy 0f definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Real")
object Restitution extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.restitution definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Restitution")
object RightTexture extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.String as Symbols.rightTexture definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#RightTexture")

object Saturation extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.saturation definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Saturation")
object Scale extends SVarDescription[simplex3d.math.floatx.ConstMat4f, simplex3d.math.floatx.ConstMat4f](simx.core.ontology.types.Transformation as Symbols.scale definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Scale")
object ScreenCoordinates extends SVarDescription[scala.collection.immutable.List[simplex3d.math.float.ConstVec2], scala.collection.immutable.List[simplex3d.math.float.ConstVec2]](simx.core.ontology.types.NullType as Symbols.screenCoordinates createdBy List[simplex3d.math.float.ConstVec2]() definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#ScreenCoordinates")
object ShadowBias extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.shadowBias definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#ShadowBias")
object ShowHeightMap extends SVarDescription[scala.Boolean, scala.Boolean](simx.core.ontology.types.Boolean as Symbols.showHeightMap definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#ShowHeightMap")
object SimulationSpeed extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.simulationSpeed definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#SimulationSpeed")
object Size extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.size definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Size")
object SkyBoxFog extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.skyBoxFog definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#SkyBoxFog")
object Sound extends EntitySVarDescription[simx.core.ontology.entities.Sound](Symbols.sound, new Sound(_) )
object SpecularColor extends SVarDescription[java.awt.Color, java.awt.Color](simx.core.ontology.types.Color as Symbols.specularColor definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#SpecularColor")
object SpotCutOff extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.spotCutOff definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#SpotCutOff")
object SpotExponent extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.spotExponent definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#SpotExponent")
object State extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.String as Symbols.state definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#State")
object String extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.NullType as Symbols.string createdBy "" definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#String")
object SubElement extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.String as Symbols.subElement definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#SubElement")

object TangibleSurface extends SVarDescription[simx.core.entity.Entity, simx.core.entity.Entity](simx.core.ontology.types.Entity as Symbols.tangibleSurface definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#TangibleSurface")
object Texture extends SVarDescription[simx.core.helper.TextureData, simx.core.helper.TextureData](simx.core.ontology.types.NullType as Symbols.texture createdBy simx.core.helper.TextureData(simplex3d.math.ConstVec2i(0,0), Array[Byte](0)) definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Texture")
object Thickness extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.thickness definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Thickness")
object Threshold extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.threshold definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Threshold")
object Time extends SVarDescription[scala.Long, scala.Long](simx.core.ontology.types.Long as Symbols.time definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Time")
object Transformation extends SVarDescription[simplex3d.math.floatx.ConstMat4f, simplex3d.math.floatx.ConstMat4f](simx.core.ontology.types.Matrix as Symbols.transformation createdBy simplex3d.math.floatx.Mat4f.Identity definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#Transformation")
object TuioCursors extends SVarDescription[scala.collection.immutable.HashMap[Symbol,simplex3d.math.float.ConstVec2], scala.collection.immutable.HashMap[Symbol,simplex3d.math.float.ConstVec2]](simx.core.ontology.types.NullType as Symbols.tuioCursors createdBy (scala.collection.immutable.HashMap[Symbol, simplex3d.math.float.ConstVec2]()) definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#TuioCursors")
object TuioObjects extends SVarDescription[scala.collection.immutable.Map[Symbol,(simplex3d.math.float.ConstVec2,Float)], scala.collection.immutable.Map[Symbol,(simplex3d.math.float.ConstVec2,Float)]](simx.core.ontology.types.NullType as Symbols.tuioObjects createdBy (scala.collection.immutable.Map[Symbol,(simplex3d.math.float.ConstVec2,Float)]()) definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#TuioObjects")

object UpTexture extends SVarDescription[java.lang.String, java.lang.String](simx.core.ontology.types.String as Symbols.upTexture definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#UpTexture")
object UpVector extends SVarDescription[simplex3d.math.floatx.ConstVec3f, simplex3d.math.floatx.ConstVec3f](simx.core.ontology.types.Vector3 as Symbols.upVector definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#UpVector")
object User extends EntitySVarDescription[simx.core.ontology.entities.User](Symbols.user, new User(_) )

object Vector2 extends SVarDescription[simplex3d.math.floatx.ConstVec2f, simplex3d.math.floatx.ConstVec2f](simx.core.ontology.types.NullType as Symbols.vector2 createdBy simplex3d.math.floatx.Vec2f.Zero definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Vector2")
object Vector3 extends SVarDescription[simplex3d.math.floatx.ConstVec3f, simplex3d.math.floatx.ConstVec3f](simx.core.ontology.types.NullType as Symbols.vector3 createdBy simplex3d.math.floatx.Vec3f.Zero definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Vector3")
object Vector4 extends SVarDescription[simplex3d.math.floatx.ConstVec4f, simplex3d.math.floatx.ConstVec4f](simx.core.ontology.types.NullType as Symbols.vector4 createdBy simplex3d.math.floatx.Vec4f.Zero definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#Vector4")
object Velocity extends SVarDescription[simplex3d.math.floatx.ConstVec3f, simplex3d.math.floatx.ConstVec3f](simx.core.ontology.types.Vector3 as Symbols.velocity definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/SimxCoreOntology.owl#Velocity")
object ViewPlatform extends SVarDescription[simplex3d.math.floatx.ConstMat4f, simplex3d.math.floatx.ConstMat4f](simx.core.ontology.types.Transformation as Symbols.viewPlatform definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#ViewPlatform")

object WaveScale extends SVarDescription[scala.Float, scala.Float](simx.core.ontology.types.Real as Symbols.waveScale definedAt "http://www.hci.uni-wuerzburg.de/ontologies/simx/concepts/BasicTypes.owl#WaveScale")
object Well extends EntitySVarDescription[simx.core.ontology.entities.Well](Symbols.well, new Well(_) )
}