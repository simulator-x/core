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

package simx.core.ontology.referencesystems

import java.io.File
import java.util.UUID

import simx.core.svaractor.{SVarActor, SVar}
import simx.core.helper.SchemaAwareXML
import simx.core.ontology.{GroundedSymbol, Symbols}
import simplex3d.math.floatx._
import xml.Node

/**
 * User: dwiebusch
 * Date: 16.05.11
 * Time: 11:24
 */

// TODO: Is this stuff used anywhere?

object CoordinateSystemConverter{
  /**
   * creates an wrapper for an already registered system
   * @param name the name of the registered system
   * @return an wrapper for the system
   */
  def apply[U](name : GroundedSymbol) : CoordinateSystemConverter[U] =
    new CoordinateSystemConverter[U](name)
}

class CoordinateSystemConverter[U] protected( name : GroundedSymbol, transform : ConstMat4f ){
  //register this system
  Coordinates.setSystem(name, transform)
  //some shortcuts to the function calls
  private val fromRef : U => U = Coordinates.convert(_, Symbols.origin, name)
  private val toRef   : U => U = Coordinates.convert(_, name, Symbols.origin)

  /**
   * Constructor for internal use (used by the companion object), to create wrappers
   * @param name the name of the aspect type to be wrapped
   */
  protected def this( name : GroundedSymbol ) =
    this(name, Coordinates.getSystem(name).getOrElse(throw new NoMappingException(name.toString)) )

  /**
   * Constructor to load system from file
   * @param name the semantics denoting the component for which this system will be registered
   * @param file the file to be loaded
   * @param nodeName the name of the xml-node parenting the nodes scale, translate[X-Z], rotate[X-Z]
   */
  def this( name : GroundedSymbol, file : File, nodeName : String ) =
    this(name, Coordinates.loadFile(file, nodeName) )

  /**
   * the alternative ctor, ensuring that no scale is entered into the rotation part of the transform
   * @param name the semantics denoting the component for which this system will be registered
   * @param rotation the rotation part of the coordinate transform
   * @param scale the scaling part of the coordinate Transform
   * @param translation the translation part of the coordinate transform
   */
  def this( name : GroundedSymbol,
            rotation : Mat4x3f  = Mat4x3f.scale(1f),
            scale    : Mat4x3f  = Mat4x3f.scale(1f),
            translation : Vec3f = Vec3f(0, 0, 0) ) =
    this(name, ConstMat4f(rotation.translate(scale * Vec4f(translation, 1))))

  /**
   * transforms local coordinates into the reference coordinate system
   * @param inLocalCoords the coordinates to be transformed
   * @return the transformed coordinates
   */
  def toRefCoords( inLocalCoords : U ) : U =
    toRef(inLocalCoords)

  /**
   * transforms reference coordinates into the local coordinate system
   * @param inRefCoords the coordinates to be transformed
   * @return the transformed coordinates
   */
  def fromRefCoords( inRefCoords : U ) : U =
    fromRef(inRefCoords)

  /**
   * transforms local coordinates into the given coordinate system
   * @param inLocalCoords the coordinates to be transformed
   * @param thatSystem the identifier denoting the other coordinate system
   * @return the transformed coordinates
   */
  def convertTo( inLocalCoords : U, thatSystem : GroundedSymbol ) : U =
    Coordinates.convert(inLocalCoords, name, thatSystem)
}

/**
 * holder for coordinate systems, providing conversion methods
 */
protected object Coordinates extends SVarActor with  ReferenceSystem[ConstMat4f]{
  private var names = Map[GroundedSymbol, UUID](Symbols.origin -> UUID.randomUUID)
  private var viewPlatformObserver : Option[SVarActor.Ref] = None

  private case class End()
  private case class Update( vpf : SVar[ConstMat4f] )
  private class ViewPlatformObserver extends SVarActor{
    private var svar : Option[SVar[ConstMat4f]] = None
    private def ignoreVP = svar.collect{ case sv => ignore(sv)  }
    private def observeVP( vpf : SVar[ConstMat4f] ) : Option[SVar[ConstMat4f]] = {
      ignoreVP
      get    (vpf)( value => setSystem( Symbols.viewPlatform, value ) )
      observe(vpf)( value => setSystem( Symbols.viewPlatform, value ) )
      Some(vpf)
    }

    addHandler[Update]{ msg => svar = observeVP(msg.vpf) }
    addHandler[End]{    msg => ignoreVP                  }
  }

  setSystem(Symbols.origin, ConstMat4f(Mat4f.Identity))
  setSystem(Symbols.viewPlatform, ConstMat4f(Mat4f.Identity))

  def loadCoordinateSetup(name : GroundedSymbol, filename : String, nodeName : String ) =
    setSystem( name, loadFile(new File(filename), nodeName) )

  def getSystem( name : GroundedSymbol ) : Option[ConstMat4f] =
    getMapping(names.getOrElse(name, throw NoMappingException(name.toString)))

  def setSystem( name : GroundedSymbol, rotation : Mat4x3f, scale : Float, translation : Vec3f ) : UUID =
    setSystem(name, ConstMat4f(rotation.translate(translation) * scale))

  def setSystem( name : GroundedSymbol, transform : ConstMat4f) : UUID = synchronized {
    names = names.updated(name, names.getOrElse(name, UUID.randomUUID))
    addMapping(names(name), transform, functions.inverse(transform))
  }

  def setViewPlatform( vpf : SVar[ConstMat4f] ) {
    if (viewPlatformObserver.isEmpty)
      viewPlatformObserver = Some(SVarActor.createActor(new ViewPlatformObserver()))
    viewPlatformObserver.get ! Update(vpf)
  }

  override def shutdown(){
    super.shutdown()
    viewPlatformObserver.collect{ case obs : SVarActor => obs.shutdown() }
  }

  //for ease of use
  def convert[U]( toConvert : U, inSystem : GroundedSymbol, outSystem : UUID ) : U =
    convert(toConvert, names.getOrElse(inSystem, throw NoMappingException(inSystem.toString)), outSystem)

  def convert[U]( toConvert : U, inSystem : UUID, outSystem : GroundedSymbol ) : U =
    convert(toConvert, inSystem, names.getOrElse(outSystem, throw NoMappingException(outSystem.toString)))

  def convert[U]( toConvert : U, inSystem : GroundedSymbol, outSystem : GroundedSymbol ) : U =
    convert(toConvert,
      names.getOrElse(inSystem, throw NoMappingException(inSystem.toString)),
      names.getOrElse(outSystem, throw NoMappingException(outSystem.toString))
    )

  def convert[U]( toConvert : U, inSystem : UUID, outSystem : UUID ) : U =
    convertTo[U, U](toConvert,
      getInverse(inSystem ).getOrElse(throw NoMappingException("id " + inSystem)),
      getMapping(outSystem).getOrElse(throw NoMappingException("id " + outSystem))
    )

  def loadFile(descFile : File, nodeName : String) : ConstMat4f =
    readTransformFromXML((SchemaAwareXML.loadFile(descFile) \ nodeName).head)

  protected def convertTo[U, V](toConvert: U, inSystem: ConstMat4f, outSystem: ConstMat4f) : V = (toConvert match {
    case value : Mat3x4f => outSystem * inSystem * value
    case value : Mat2x4f => outSystem * inSystem * value
    case value : Mat4f   => outSystem * inSystem * value
    case value : Vec4f   => outSystem * inSystem * value
    case _                => throw NoConversionPossibleException(toConvert)
  }).asInstanceOf[V]

  private def readTransformFromXML(n: Node) : ConstMat4f = {
    val translateX = (n \ "translateX").text.toFloat
    val translateY = (n \ "translateY").text.toFloat
    val translateZ = (n \ "translateZ").text.toFloat

    val rotateX = (n \ "rotateX").text.toFloat
    val rotateY = (n \ "rotateY").text.toFloat
    val rotateZ = (n \ "rotateZ").text.toFloat

    val scale = (n \ "scale").text.toFloat

    ConstMat4f( Mat4x3f.
      rotateX(functions.radians(rotateX)).rotateY(functions.radians(rotateY)).rotateZ(functions.radians(rotateZ)).
      scale(scale).translate( Vec3f(translateX, translateY, translateZ) )
    )
  }
}
