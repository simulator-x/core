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

package simx.core.components.renderer.setup

import simplex3d.math.floatx.ConstMat4f
import simx.core.component.{Triggered, Unbound, Frequency}
import simplex3d.math.float._
import simx.core.component.Unbound
import simx.core.component.Triggered

/**
 * This enumeration describes different link types between different logical displays.
 *
 * @author Dennis Wiebusch
 * @author Stephan Rehfeld
 */

object LinkType extends Enumeration {

  /**
   * This values describes that two display belong together and should be rendererd in one window in anaglyph mode.
   */
  val AnaglyphStereo = Value( "AnaglyphStereo" )

  /**
   * This values describes that two displays belong together and that the different frame are display in a frame
   * sequential mode.
   */
  val FrameSequential = Value( "FrameSequential" )

  /**
   * This value describes that the displays are single displays.
   */
  val SingleDisplay = Value( "SingleDisplay" )

  /**
   * This value describes that the displays are single displays.
   */
  val TopBottomStereo = Value( "TopBottomStereo" )
}

/**
 * This enumeration contains the possible eyes of a user.
 * @author Dennis Wiebusch
 * @author Stephan Rehfeld
 */
object Eye extends Enumeration  {

  /**
   * The left eye.
   */
  val LeftEye = Value( "LeftEye" )

  /**
   * The right eye.
   */
  val RightEye = Value( "RightEye" )
}

/**
 * This class describes a camera. It can also be interpreted as user.
 *
 * @author Dennis Wiebusch
 * @author Stephan Rehfeld
 *
 * @param camId The id of the camera/user.
 * @param eye The eye that is drawn on the screen.
 * @param eyeSeparation An option separation between the eyes of the user. The default value assumed by moth renderer is 0.07cm.
 */
class CamDesc( val camId : Int, val eye : Eye.Value, val eyeSeparation : Option[Float] = None ) extends Serializable {
  require( eye != null, "The parameter 'eye' must not be 'null'!")
  require( eyeSeparation != null, "The parameter 'eyeSeparation' must not be 'null'!")
}


/**
 * This class describes on display.
 *
 * @author Dennis Wiebusch
 * @author Stephan Rehfeld
 *
 * @param resolution A optional resolution of the display. None means full screen.
 * @param size The width and height of the screen in meters.
 * @param transformation The physical transformation of the screen relative to the view platform (like center of the CAVE).
 * @param view The description of the user, which eye, which seperation between the eyes.
 */
class DisplayDesc( val resolution: Option[(Int, Int)], val size: (Double, Double), val transformation: ConstMat4f, val view: CamDesc ) extends Serializable {
  def sizeVec = ConstVec2(size._1.toFloat, size._2.toFloat)
}

/**
 * This class respresent a hardware display device.
 *
 * @author Dennis Wiebusch
 * @author Stephan Rehfeld
 *
 * @param hardwareHandle A optional hardware handle. The meanings are (display, screen, channel). If it is not set it depends on the OS where the window is opened.
 * @param displayDescs The displays behind this hardware handle.
 * @param linkType A description how the displays belongs together.
 * @param node The node where this device is connected to. Default is None and means local node.
 */
class DisplayDevice( val hardwareHandle: Option[(Int, Int, Int)], val displayDescs : List[DisplayDesc], val linkType : LinkType.Value = LinkType.SingleDisplay, val node : Option[Symbol] = None ) extends Serializable
{
  def this( hardwareHandle: Option[(Int, Int, Int)],firstDpy : DisplayDesc, linkType : LinkType.Value, node : Option[Symbol] ) = this( hardwareHandle, firstDpy :: Nil, linkType, node )
}

/**
 * This class represents a display device group. All displays devices in the same group gets redrawn at the same time.
 *
 * @author Dennis Wiebusch
 * @author Stephan Rehfeld
 *
 * @param dpys A set of display.
 *
 */
class DisplayDeviceGroup( var dpys : Set[DisplayDevice] ) extends Serializable {

  /**
   * This method adds a display device to this display device group.
   *
   * @param dev The display device to add.
   */
  def addDevice( dev : DisplayDevice ) {
    dpys += dev
  }
}

/**
 * This class represents a display setup. I can be interpreted by a renderer connection to setup up windows
 * on every display that shows the scene.
 */
class DisplaySetupDesc extends Serializable {

  /**
   * The device groups of this display setup.
   */
  var deviceGroups = Map[Int, DisplayDeviceGroup]()

  /**
   * A map that holds the frequency for the device groups.
   */
  var frequencies = Map[Int,Frequency]()

  /**
   * This methods adds a display device to a display device group.
   *
   * @param groupId The id of the display device group the display device belongs to.
   * @return the display setup desc itself
   */
  def addDevice( dev : DisplayDevice , groupId : Int ) : DisplaySetupDesc = {
    val updated = deviceGroups.getOrElse(groupId, new DisplayDeviceGroup(Set[DisplayDevice]()))
    updated.addDevice(dev)
    deviceGroups = deviceGroups.updated(groupId, updated)
    this
  }

  /**
   * This method sets the frequency of a device group. [[simx.core.component.Triggered]] means, that the group is
   * triggered when the rendering component is triggered. This is the default behavior. [[simx.core.component.Unbound]]
   * means that the group is running as fast, as it can.
   *
   * @param groupId The id of the device group.
   * @param frequency The frequency.
   */
  def setFrequency( groupId : Int, frequency : Frequency ) {
    require( frequency.isInstanceOf[Triggered] || frequency.isInstanceOf[Unbound], "The frequency must either be Triggered or Unbound!" )
    frequencies = frequencies + (groupId -> frequency )
  }

  /**
   * This method returns the frequency of a given device group. The default value is [[simx.core.component.Triggered]].
   *
   * @param groupId The group id of the device group.
   * @return The freuency of the device group.
   */
  def getFrequencyOf( groupId : Int ) = this.frequencies.getOrElse( groupId, Triggered() )

  /**
   * Returns the first display if existent.
   * @return
   */
  def firstDisplay: DisplayDesc =
    try{deviceGroups.head._2.dpys.head.displayDescs.head} catch {
      case _: Throwable => throw new Exception("[DisplaySetupDesc] At least one display is required.")
    }

}