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

package simx.core.components.renderer.createparameter

import simplex3d.math.floatx._
import simx.core.ontology.{Symbols, GroundedSymbol}
import simx.core.ontology.types._
import java.awt.Color
import simx.core.entity.Entity
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.description.{NamedSValSet, EntityAspect, SValSeq}
import simplex3d.math.float._

/**
 * A flag that indicates that this value (like a transform) is provided by another create parameter.
 *
 * @author Stephan Rehfeld
 */
class ReadFromElseWhere extends Serializable

/**
 * A flag that indicates that this value (like a transform) is provided by another create parameter.
 *
 * @author Stephan Rehfeld
 */
object ReadFromElseWhere extends ReadFromElseWhere

/**
 * This package contains several implicit converter functions to use the create parameter for the renderer in a more
 * comfortable way.
 *
 * @author Stephan Rehfeld
 */
package object convert {
  import scala.language.implicitConversions
  implicit def constMat4ToRightEither( transformation : ConstMat4f ) : Right[ReadFromElseWhere,ConstMat4f] = Right( transformation )
  implicit def color4ToRightEither( color : Color ) : Right[ReadFromElseWhere,Color] = Right( color )
  implicit def float4ToRightEither( float : Float ) : Right[ReadFromElseWhere,Float] = Right( float )
  implicit def boolean4ToRightEither( boolean : Boolean ) : Right[ReadFromElseWhere,Boolean] = Right( boolean )
  implicit def readFromElseWhereToLeftEither( readFromElseWhere : ReadFromElseWhere ) : Left[ReadFromElseWhere,ReadFromElseWhere] = Left( ReadFromElseWhere )
}

/**
 * A base class for all renderer create parameter.
 *
 * @author Stephan Rehfeld
 * @author Dennis Wiebusch
 *
 * @param aspectType The type of the create parameter.
 * @param targets A list of target components for this aspect.
 */
abstract class RendererAspect( aspectType : GroundedSymbol, targets : List[Symbol] = Nil )
  extends EntityAspect( Symbols.graphics, aspectType, targets ) with Serializable {
  def getProvidings =
    getFeatures
}

/**
 * This class represents the create param for a new spot light.
 *
 * @author Stephan Rehfeld
 *
 * @param name The name of the light withing the internal scene graph. Mandatory parameter.
 * @param transformation The transformation of the light. Mandatory parameter.
 * @param parentElement An optional parent element. The light will be added under the parent element in the scene graph.
 * @param diffuseColor The diffuse color of the light. Default color is white.
 * @param specularColor The specular color of the light. Default color is white.
 * @param constantAttenuation The constant attenuation of the light. Default value is 1.0.
 * @param linearAttenuation The linear attenuation of the light. Default value is 0.0.
 * @param quadraticAttenuation The quadratic attenuation of the light. Default value is 0.0.
 * @param spotCutOff The spot cut off angle of the light. Default value is 30.
 * @param spotExponent The spot exponent of the light. Default value is 1.
 * @param castShadow A flag that tells if this light casts a shadow. Default value if true.
 * @param shadowBias Default value is 0.25.
 */
case class SpotLight( name: java.lang.String,
                      transformation : Either[ReadFromElseWhere,ConstMat4f] = Right( ConstMat4f( Mat3x4f.Identity ) ),
                      parentElement : Option[ Entity ] = None,
                      diffuseColor : Either[ReadFromElseWhere,Color] = Right( Color.WHITE ),
                      specularColor : Either[ReadFromElseWhere,Color] = Right( Color.WHITE ),
                      constantAttenuation : Either[ReadFromElseWhere,Float] = Right( 1.0f ),
                      linearAttenuation : Either[ReadFromElseWhere,Float] = Right( 0.0f ),
                      quadraticAttenuation : Either[ReadFromElseWhere,Float] = Right( 0.0f ),
                      spotCutOff : Either[ReadFromElseWhere,Float] = Right( 30.0f ),
                      spotExponent : Either[ReadFromElseWhere,Float] = Right( 1.0f ),
                      castShadow : Either[ReadFromElseWhere,scala.Boolean] = Right( true ),
                      shadowBias : Either[ReadFromElseWhere,Float] = Right( 0.25f ) ) extends RendererAspect( Symbols.spotLight ) {

  require( name != null, "The parameter 'name' must not be 'null'!" )
  require( transformation != null, "The parameter 'transformation' must not be 'null'!" )
  require( (transformation.isLeft && transformation.left.get != null) || (transformation.isRight && transformation.right.get != null), "Neither left or right value of parameter 'transformation' must not be 'null'!" )
  require( parentElement != null, "The parameter 'parentElement' must not be 'null'!" )
  require( parentElement.isEmpty || parentElement.get != null, "The parameter 'parentElement' must not be 'null'!" )
  require( diffuseColor != null, "The parameter 'diffuseColor' must not be 'null'!" )
  require( (diffuseColor.isLeft && diffuseColor.left.get != null) || (diffuseColor.isRight && diffuseColor.right.get != null), "Neither left or right value of parameter 'diffuseColor' must not be 'null'!" )
  require( specularColor != null, "The parameter 'specularColor' must not be 'null'!" )
  require( (specularColor.isLeft && specularColor.left.get != null) || (specularColor.isRight && specularColor.right.get != null), "Neither left or right value of parameter 'specularColor' must not be 'null'!" )
  require( constantAttenuation != null, "The parameter 'constantAttenuation' must not be 'null'!" )
  require( (constantAttenuation.isLeft && constantAttenuation.left.get != null) || constantAttenuation.isRight, "Neither left or right value of parameter 'constantAttenuation' must not be 'null'!" )
  require( linearAttenuation != null, "The parameter 'linearAttenuation' must not be 'null'!" )
  require( (linearAttenuation.isLeft && linearAttenuation.left.get != null) || linearAttenuation.isRight, "Neither left or right value of parameter 'linearAttenuation' must not be 'null'!" )
  require( quadraticAttenuation != null, "The parameter 'quadraticAttenuation' must not be 'null'!" )
  require( (quadraticAttenuation.isLeft && quadraticAttenuation.left.get != null) || quadraticAttenuation.isRight, "Neither left or right value of parameter 'quadraticAttenuation' must not be 'null'!" )
  require( spotCutOff != null, "The parameter 'spotCutOff' must not be 'null'!" )
  require( (spotCutOff.isLeft && spotCutOff.left.get != null) || spotCutOff.isRight, "Neither left or right value of parameter 'spotCutOff' must not be 'null'!" )
  require( spotExponent != null, "The parameter 'spotExponent' must not be 'null'!" )
  require( (spotExponent.isLeft && spotExponent.left.get != null) || spotExponent.isRight, "Neither left or right value of parameter 'spotExponent' must not be 'null'!" )
  require( castShadow != null, "The parameter 'castShadow' must not be 'null'!" )
  require( (castShadow.isLeft && castShadow.left.get != null) || castShadow.isRight, "Neither left or right value of parameter 'castShadow' must not be 'null'!" )
  require( shadowBias != null, "The parameter 'shadowBias' must not be 'null'!" )
  require( (shadowBias.isLeft && shadowBias.left.get != null) || shadowBias.isRight, "Neither left or right value of parameter 'shadowBias' must not be 'null'!" )

  override def getCreateParams = {
    var cVars = new SValSeq
    cVars = cVars and Name( name )
    if( transformation.isRight ) cVars = cVars and Transformation( transformation.right.get )
    if( constantAttenuation.isRight ) cVars = cVars and ConstantAttenuation( constantAttenuation.right.get )
    if( linearAttenuation.isRight ) cVars = cVars and LinearAttenuation( linearAttenuation.right.get )
    if( quadraticAttenuation.isRight ) cVars = cVars and QuadraticAttenuation( quadraticAttenuation.right.get )
    if( spotCutOff.isRight ) cVars = cVars and SpotCutOff( spotCutOff.right.get )
    if( spotExponent.isRight ) cVars = cVars and SpotExponent( spotExponent.right.get )
    if( castShadow.isRight ) cVars = cVars and CastShadow( castShadow.right.get )
    if( shadowBias.isRight ) cVars = cVars and ShadowBias( shadowBias.right.get )
    if( diffuseColor.isRight ) cVars = cVars and DiffuseColor( diffuseColor.right.get )
    if( specularColor.isRight ) cVars = cVars and SpecularColor( specularColor.right.get )
    if( parentElement.isDefined ) cVars = cVars and ParentElement( parentElement.get )
    addCVars( cVars )
  }

  override def getFeatures : Set[ConvertibleTrait[_]] = {
    var features = Set[ConvertibleTrait[_]]()
    features = features +  Transformation
    features = features +  ConstantAttenuation
    features = features +  LinearAttenuation
    features = features +  QuadraticAttenuation
    features = features +  SpotCutOff
    features = features +  SpotExponent
    features = features +  CastShadow
    features = features +  ShadowBias
    features = features +  DiffuseColor
    features = features +  SpecularColor
    features
  }

  override def getProvidings : Set[ConvertibleTrait[_]] = {
    var providings = Set[ConvertibleTrait[_]]()
    if( transformation.isRight ) providings = providings +  Transformation
    if( constantAttenuation.isRight ) providings = providings +  ConstantAttenuation
    if( linearAttenuation.isRight ) providings = providings +  LinearAttenuation
    if( quadraticAttenuation.isRight ) providings = providings +  QuadraticAttenuation
    if( spotCutOff.isRight ) providings = providings +  SpotCutOff
    if( spotExponent.isRight ) providings = providings +  SpotExponent
    if( castShadow.isRight ) providings = providings +  CastShadow
    if( castShadow.isRight ) providings = providings +  ShadowBias
    if( diffuseColor.isRight ) providings = providings +  DiffuseColor
    if( specularColor.isRight ) providings = providings +  SpecularColor
    providings
  }

}

/**
 * This class represents the create param for a new point light.
 *
 * @author Stephan Rehfeld
 *
 * @param name The name of the light withing the internal scene graph. Mandatory parameter.
 * @param transformation The transformation of the light. Mandatory parameter.
 * @param parentElement An optional parent element. The light will be added under the parent element in the scene graph.
 * @param diffuseColor The diffuse color of the light. Default color is white.
 * @param specularColor The specular color of the light. Default color is white.
 * @param constantAttenuation The constant attenuation of the light. Default value is 1.0.
 * @param linearAttenuation The linear attenuation of the light. Default value is 0.0.
 * @param quadraticAttenuation The quadratic attenuation of the light. Default value is 0.0.
 */
case class PointLight( name: java.lang.String,
                       transformation : Either[ReadFromElseWhere,ConstMat4f] = Right( ConstMat4f( Mat3x4f.Identity ) ),
                       parentElement : Option[ Entity ] = None,
                       diffuseColor : Either[ReadFromElseWhere,Color] = Right( Color.WHITE ),
                       specularColor : Either[ReadFromElseWhere,Color] = Right( Color.WHITE ),
                       constantAttenuation : Either[ReadFromElseWhere,Float] = Right( 1.0f ),
                       linearAttenuation : Either[ReadFromElseWhere,Float] = Right( 0.0f ),
                       quadraticAttenuation : Either[ReadFromElseWhere,Float] = Right( 0.0f )
                     ) extends RendererAspect( Symbols.pointLight ) {

  require( name != null, "The parameter 'name' must not be 'null'!" )
  require( transformation != null, "The parameter 'transformation' must not be 'null'!" )
  require( (transformation.isLeft && transformation.left.get != null) || (transformation.isRight && transformation.right.get != null), "Neither left or right value of parameter 'transformation' must not be 'null'!" )
  require( parentElement != null, "The parameter 'parentElement' must not be 'null'!" )
  require( parentElement.isEmpty || parentElement.get != null, "The parameter 'parentElement' must not be 'null'!" )
  require( diffuseColor != null, "The parameter 'diffuseColor' must not be 'null'!" )
  require( (diffuseColor.isLeft && diffuseColor.left.get != null) || (diffuseColor.isRight && diffuseColor.right.get != null), "Neither left or right value of parameter 'diffuseColor' must not be 'null'!" )
  require( specularColor != null, "The parameter 'specularColor' must not be 'null'!" )
  require( (specularColor.isLeft && specularColor.left.get != null) || (specularColor.isRight && specularColor.right.get != null), "Neither left or right value of parameter 'specularColor' must not be 'null'!" )
  require( constantAttenuation != null, "The parameter 'constantAttenuation' must not be 'null'!" )
  require( (constantAttenuation.isLeft && constantAttenuation.left.get != null) || constantAttenuation.isRight, "Neither left or right value of parameter 'constantAttenuation' must not be 'null'!" )
  require( linearAttenuation != null, "The parameter 'linearAttenuation' must not be 'null'!" )
  require( (linearAttenuation.isLeft && linearAttenuation.left.get != null) || linearAttenuation.isRight, "Neither left or right value of parameter 'linearAttenuation' must not be 'null'!" )
  require( quadraticAttenuation != null, "The parameter 'quadraticAttenuation' must not be 'null'!" )
  require( (quadraticAttenuation.isLeft && quadraticAttenuation.left.get != null) || quadraticAttenuation.isRight, "Neither left or right value of parameter 'quadraticAttenuation' must not be 'null'!" )


  override def getCreateParams = {
    var cVars = new SValSeq
    cVars = cVars and Name( name )
    if( transformation.isRight ) cVars = cVars and Transformation( transformation.right.get )
    if( constantAttenuation.isRight ) cVars = cVars and ConstantAttenuation( constantAttenuation.right.get )
    if( linearAttenuation.isRight ) cVars = cVars and LinearAttenuation( linearAttenuation.right.get )
    if( quadraticAttenuation.isRight ) cVars = cVars and QuadraticAttenuation( quadraticAttenuation.right.get )
    if( diffuseColor.isRight ) cVars = cVars and DiffuseColor( diffuseColor.right.get )
    if( specularColor.isRight ) cVars = cVars and SpecularColor( specularColor.right.get )
    if( parentElement.isDefined ) cVars = cVars and ParentElement( parentElement.get )
    addCVars( cVars )
  }

  override def getFeatures : Set[ConvertibleTrait[_]] = {
    var features = Set[ConvertibleTrait[_]]()
    features = features +  Transformation
    features = features +  ConstantAttenuation
    features = features +  LinearAttenuation
    features = features +  QuadraticAttenuation
    features = features +  DiffuseColor
    features = features +  SpecularColor
    features
  }

  override def getProvidings : Set[ConvertibleTrait[_]] = {
    var providings = Set[ConvertibleTrait[_]]()
    if( transformation.isRight ) providings = providings +  Transformation
    if( constantAttenuation.isRight ) providings = providings +  ConstantAttenuation
    if( linearAttenuation.isRight ) providings = providings +  LinearAttenuation
    if( quadraticAttenuation.isRight ) providings = providings +  QuadraticAttenuation
    if( diffuseColor.isRight ) providings = providings +  DiffuseColor
    if( specularColor.isRight ) providings = providings +  SpecularColor
    providings
  }

}

/**
 * This create parameter represents a sky box.
 *
 * @author Stephan Rehfeld
 *
 * @param name The name of the sky box.
 * @param frontTexture The texture of the front side of the sky box.
 * @param backTexture The texture of the back side of the sky box.
 * @param leftTexture  The texture of the left side of the sky box.
 * @param rightTexture The texture of the right side of the sky box.
 * @param topTexture The texture of the top side of the sky box.
 * @param bottomTexture The texture of the bottom side of the sky box.
 */
case class SkyBox( name: java.lang.String,
                   frontTexture : java.lang.String,
                   backTexture : java.lang.String,
                   leftTexture : java.lang.String,
                   rightTexture : java.lang.String,
                   topTexture : java.lang.String,
                   bottomTexture : java.lang.String
                  ) extends RendererAspect( Symbols.skyBox ) {
  require( name != null, "The parameter 'name' must not be 'null'!" )
  require( frontTexture != null, "The parameter 'frontTexture' must not be 'null'!" )
  require( backTexture != null, "The parameter 'backTexture' must not be 'null'!" )
  require( leftTexture != null, "The parameter 'leftTexture' must not be 'null'!" )
  require( rightTexture != null, "The parameter 'rightTexture' must not be 'null'!" )
  require( topTexture != null, "The parameter 'topTexture' must not be 'null'!" )
  require( bottomTexture != null, "The parameter 'bottomTexture' must not be 'null'!" )

  override def getCreateParams = addCVars {
      Name( name ) and
      FrontTexture( frontTexture ) and
      BackTexture( backTexture ) and
      LeftTexture( leftTexture ) and
      RightTexture( rightTexture ) and
      UpTexture( topTexture ) and
      DownTexture( bottomTexture )

  }

  override def getFeatures = Set()

}

/**
 * This create parameter creates an object that is loaded from a file.
 *
 * @author Stephan Rehfeld
 *
 * @param file The file name to load.
 * @param subElement Optional the name of a sub element in the loaded scene graph that should be representation of the entity.
 * @param parentElement An optional entity that represents paranent element in the scene graph.
 * @param transformation The transformation of the element for ReadFromElseWhere if another component provides this parameter. Default value is the identity matrix.
 * @param scale A scale factor for the element. Default value is no scale factor.
 */
case class ShapeFromFile( file: java.lang.String,
                          subElement : Option[java.lang.String] = None,
                          parentElement : Option[Entity] = None,
                          transformation : Either[ReadFromElseWhere,ConstMat4f]  = Right( ConstMat4f( Mat3x4f.Identity ) ),
                          scale : ConstMat4f = ConstMat4f( Mat3x4f.Identity )
                        ) extends RendererAspect( Symbols.shapeFromFile ) {

  require( file != null, "The parameter 'file' must not be 'null'!" )
  require( subElement != null, "The parameter ' subElement' must not be 'null'!")

  override def getCreateParams = {
    var cVars = new SValSeq
    cVars = cVars and ColladaFile( file )
    if( subElement.isDefined ) cVars = cVars and SubElement( subElement.get )
    if( transformation.isRight ) cVars = cVars and Transformation( transformation.right.get )
    cVars = cVars and Scale( scale )
    addCVars( cVars )
  }

  override def getFeatures : Set[ConvertibleTrait[_]] = {
    var features = Set[ConvertibleTrait[_]]()
    features = features +  Transformation
    features
  }

  override def getProvidings : Set[ConvertibleTrait[_]] = {
    var providings = Set[ConvertibleTrait[_]]()
    if( transformation.isRight ) providings = providings +  Transformation
    providings
  }

}

case class GroupNode(trafo : ConstMat4, scale : ConstVec3 = Vec3.One) extends RendererAspect(Symbols.parentElement){
  /**
   * the features the entity will at least have when it is created
   * @return the features the entity will at least have when it is created
   */
  override def getFeatures: Set[ConvertibleTrait[_]] = Set(Transformation, Scale)

  /**
   *
   * The list of create parameters
   * @return a list of [[simx.core.entity.description.SVal]]'s containing the information needed to instantiate an
   *         entity with this aspect
   */
  override def getCreateParams: NamedSValSet = addCVars(Transformation(trafo) and Scale(ConstMat4(Mat4x3.scale(scale))))
}

/**
 * This create parameter takes and existing node from the currently active scene graph and provides it as an enitity
 * to the middleware.
 *
 * @author Stephan Rehfeld
 *
 * @param subElement The name of the element in the scene graph.
 * @param scale A scale factor for this element.
 */
case class ExistingNode(
                         subElement: java.lang.String,
                         scale : ConstMat4f = ConstMat4f( Mat3x4f.Identity )
                 ) extends RendererAspect( Symbols.existingNode ) {

  override def getCreateParams = addCVars {
      SubElement( subElement ) and Scale( scale )
  }

  override def getFeatures : Set[ConvertibleTrait[_]] = {
    var features = Set[ConvertibleTrait[_]]()
    features = features +  Transformation
    features
  }

  override def getProvidings : Set[ConvertibleTrait[_]] = {
    var providings = Set[ConvertibleTrait[_]]()
    providings = providings +  Transformation
    providings
  }

}

/**
 * This create parameter creates the use of the user. The state variables in this entity controls the position
 * and head transform of the camera.
 *
 * @author Dennis Wiebusch
 *
 * @param viewPlatform The position of the user. Default value is the identity matrix.
 * @param headTransform The head transform of the user. Default value is the identity matrix.
 */
case class VRUser(  viewPlatform : ConstMat4f = ConstMat4f( Mat3x4f.Identity ),
                    headTransform : ConstMat4f = ConstMat4f( Mat3x4f.Identity )

                   ) extends RendererAspect( Symbols.user ) {
  override def getCreateParams = addCVars {
    ViewPlatform( viewPlatform ) and HeadTransform( headTransform )
  }

  override def getFeatures : Set[ConvertibleTrait[_]] = {
    var features = Set[ConvertibleTrait[_]]()
    features = features +  ViewPlatform
    features = features +  HeadTransform
    features
  }

}

/*
 * -------------------------------------------------------------------
 * SEVERAL SIMTHIEF SPECIFIC EFFECTS THAT WILL BE REMOVED IN FUTURE AS
 * THEY CAN BE EXPRESS BY USING THE EFFECT DSLs.
 *
 * -------------------------------------------------------------------
 */

case class Mirror( name: java.lang.String,
                   file : java.lang.String,
                   transformation : Either[ReadFromElseWhere,ConstMat4f] = Right( ConstMat4f( Mat3x4f.Identity ) )
                   ) extends RendererAspect( Symbols.mirror ) {

  require( name != null, "The parameter 'name' must not be 'null'!" )
  require( file != null, "The parameter 'file' must not be 'null'!" )
  require( transformation != null, "The parameter 'transformation' must not be 'null'!" )
  require( (transformation.isLeft && transformation.left.get != null) || (transformation.isRight && transformation.right.get != null), "Neither left or right value of parameter 'transformation' must not be 'null'!" )

  override def getCreateParams = {
    var cVars = new SValSeq
    cVars = cVars and Name( name )
    cVars = cVars and File( file )
    if( transformation.isRight ) cVars = cVars and Transformation( transformation.right.get )
    addCVars( cVars )
  }

  override def getFeatures : Set[ConvertibleTrait[_]] = {
    var features = Set[ConvertibleTrait[_]]()
    features = features +  Transformation
    features
  }

  override def getProvidings : Set[ConvertibleTrait[_]] = {
    var providings = Set[ConvertibleTrait[_]]()
    if( transformation.isRight ) providings = providings +  Transformation
    providings
  }

}

case class Water( name: java.lang.String,
                  file : java.lang.String,
                  transformation : Either[ReadFromElseWhere,ConstMat4f] = Right( ConstMat4f( Mat3x4f.Identity ) ),
                  waveScale : Either[ReadFromElseWhere,Float]
                  ) extends RendererAspect( Symbols.water ) {

  require( name != null, "The parameter 'name' must not be 'null'!" )
  require( file != null, "The parameter 'file' must not be 'null'!" )
  require( transformation != null, "The parameter 'transformation' must not be 'null'!" )
  require( (transformation.isLeft && transformation.left.get != null) || (transformation.isRight && transformation.right.get != null), "Neither left or right value of parameter 'transformation' must not be 'null'!" )
  require( waveScale != null, "The parameter 'waveScale' must not be 'null'!" )
  require( (waveScale.isLeft && waveScale.left.get != null) || waveScale.isRight, "Neither left or right value of parameter 'waveScale' must not be 'null'!" )


  override def getCreateParams = {
    var cVars = new SValSeq
    cVars = cVars and Name( name )
    cVars = cVars and File( file )
    if( transformation.isRight ) cVars = cVars and Transformation( transformation.right.get )
    if( waveScale.isRight ) cVars = cVars and WaveScale( waveScale.right.get )
    addCVars( cVars )
  }

  override def getFeatures : Set[ConvertibleTrait[_]] = {
    var features = Set[ConvertibleTrait[_]]()
    features = features +  Transformation
    features = features +  WaveScale
    features
  }

  override def getProvidings : Set[ConvertibleTrait[_]] = {
    var providings = Set[ConvertibleTrait[_]]()
    if( transformation.isRight ) providings = providings +  Transformation
    if( waveScale.isRight ) providings = providings +  WaveScale
    providings
  }

}

/*
 * --------------------------------------
 *  MAYBE IN FUTURE SUPPORTED
 *
 *
 * ------------------------------------
 */
case class AnimatedObject( //name: java.lang.String,
                           subElement : Option[java.lang.String] = None,
                           parentElement : Option[Entity] = None,
                           transformation : ConstMat4f  =  ConstMat4f( Mat3x4f.Identity ) ,
                           scale : ConstMat4 = Mat4f.Identity//,
//                            texture : TextureData = TextureData.apply(Color.RED)
                        ) extends RendererAspect( Symbols.meshComponent ) {

//  require( name != null, "The parameter 'name' must not be 'null'!" )
  require( subElement != null, "The parameter ' subElement' must not be 'null'!")
  require( parentElement != null, "The parameter ' subElement' must not be 'null'!")
  require( transformation != null, "The parameter ' subElement' must not be 'null'!")

  override def getCreateParams = {
    var cVars = new SValSeq
//    cVars = cVars and Name( name )
    if( subElement.isDefined ) cVars = cVars and SubElement( subElement.get )
    if( parentElement.isDefined ) cVars = cVars and ParentElement( parentElement.get )
    cVars = cVars and Transformation( transformation )
    cVars = cVars and Scale( scale )
//    cVars = cVars and Texture(texture)
    addCVars( cVars )
  }

  override def getFeatures : Set[ConvertibleTrait[_]] = {
    var features = Set[ConvertibleTrait[_]]()
    features = features + Transformation
    features = features + simx.core.ontology.types.Mesh
    features = features + simx.core.ontology.types.Texture
    features
  }

  override def getProvidings : Set[ConvertibleTrait[_]] = {
    var providings = Set[ConvertibleTrait[_]]()
    providings = providings +  Transformation
    providings
  }

}