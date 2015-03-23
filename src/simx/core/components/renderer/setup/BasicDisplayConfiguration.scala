/*
 * Copyright 2014 The SIRIS Project
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

package simx.core.components.renderer.setup

import simplex3d.math.floatx.{Vec3f, Mat4x3f, ConstMat4f}
import simplex3d.math.float.ConstMat4

/**
 * This object creates a display description for single display rendering setup.
 *
 * @author Dennis Wiebusch
 */
object BasicDisplayConfiguration {

  /**
   * Creates a display setup description with two nodes that should fit for every desktop computer or laptop.
   *
   * @param widthInPx The amount of pixel in x direction. Must be larger than 0.
   * @param heightInPx The amount of pixel in y direction. Must be larger than 0.
   * @param fullscreen An optional parameter that sets if the application runs in fullscreen mode. Default value is false.
   * @param dpi The dots per inch of the screen. Must be larger than 0.0.
   * @return The display description with two windows on two different nodes.
   */
  def apply(widthInPx: Int, heightInPx: Int, fullscreen : Boolean = false, dpi : Double = 48.0) : DisplaySetupDesc = {
    require( widthInPx > 0, "The parameter 'widthInPx' must be larger than 0!" )
    require( heightInPx > 0, "The parameter 'heightInPx' must be larger than 0!" )
    require( dpi > 0.0, "The parameter 'dpi' must be larger than 0!" )

    val widthOfScreenInMeters = widthInPx / dpi * 0.0254
    val displayDesc = new DisplayDesc(
      if (fullscreen) None else Some( widthInPx -> heightInPx ),
      widthOfScreenInMeters -> widthOfScreenInMeters * heightInPx / widthInPx,
      ConstMat4f( Mat4x3f.translate( Vec3f( 0.0f, 0.0f, -0.6f ) ) ),
      new CamDesc( 0, Eye.RightEye, Some( 0.0f ) )
    )
    new DisplaySetupDesc().addDevice( new DisplayDevice( None, displayDesc :: Nil, LinkType.SingleDisplay ), 0 )
  }
}

class ConfigurableDisplayConfiguration(val initialFullscreen : Boolean = false) {
  private var fullscreen = initialFullscreen
  private var resolution = (800, 600)
  private var sizeOfScreen = (0.8 , 0.45)
  private var linkType = LinkType.SingleDisplay
  private var hardwareHandle : Option[(Int, Int, Int)] = None
  private var transformation =  ConstMat4(Mat4x3f.translate( Vec3f( 0.0f, 0.0f, -0.6f ) ) )

  override def toString : String = {
    "DisplayConfig with \n\t" +
      "sizeOfScreen\t = " + sizeOfScreen._1 + "m x " + sizeOfScreen._2 + "m (W x H)\n\t" +
      "resolution\t\t = " + resolution._1 + " x " + resolution._2 + " (W x H)\n\t" +
      "fullscreen\t\t = " + fullscreen + "\n\t" +
      "linkType\t\t = " + linkType + "\n\t" +
      "hardwareHandle\t = " + (if (hardwareHandle.isDefined) "display: " + hardwareHandle.get._1 + ", screen: " + hardwareHandle.get._2 + ", channel: " + hardwareHandle.get._3 else "undefined") + "\n\t" +
      "transformInMetersRelativeToWorldRoot = \n " + transformation
  }

  def setFullscreen(enabled : Boolean) {
    fullscreen = enabled
  }

  def setResolution(width : Int, height : Int){
    resolution = width -> height
  }

  def setSizeOfScreen(widthInMeters : Double, heightInMeters : Double){
    sizeOfScreen = widthInMeters -> heightInMeters
  }

  def setLinkType(link : LinkType.Value){
    linkType = link
  }

  def setHardwareHandle(display : Int, screen : Int, channel : Int){
    hardwareHandle = Some((display, screen, channel))
  }

  def setScreenTransform(transformInMetersRelativeToWorldRoot : ConstMat4){
    transformation = transformInMetersRelativeToWorldRoot
  }

  def clearHardwareHandle(){
    hardwareHandle = None
  }

  def getScreenTransform =
    transformation

  def getFullscreen =
    fullscreen

  def getResolution =
    resolution

  def getSizeOfScreen =
    sizeOfScreen

  def getLinkType =
    linkType

  def getHardwareHandle =
    hardwareHandle

  def getDescription = {
    //TODO: support multiple cams
    val cams = linkType match {
      case LinkType.AnaglyphStereo => new CamDesc( 0, Eye.RightEye, Some( 0.06f ) ) :: Nil
      case LinkType.FrameSequential => new CamDesc( 0, Eye.RightEye, Some( 0.0f ) ) :: Nil
      case LinkType.SingleDisplay => new CamDesc( 0, Eye.RightEye, Some( 0.0f ) ) :: Nil
      case LinkType.TopBottomStereo => new CamDesc( 0, Eye.RightEye, Some( 0.06f ) ) :: Nil
    }


    var displayDescs = new DisplayDesc(
      if (fullscreen) None else Some( resolution ),
      sizeOfScreen,
      transformation,
      cams.head ) :: Nil
    if (cams.size == 2)
      displayDescs = new DisplayDesc(
        if (fullscreen) None else Some( resolution ),
        sizeOfScreen,
        transformation,
        cams.head ) :: displayDescs
    new DisplaySetupDesc().addDevice( new DisplayDevice( hardwareHandle, displayDescs, linkType ), 0 )
  }
}
