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

package simx.core.helper

import java.io.{FileOutputStream, OutputStreamWriter, BufferedWriter, File}

import simplex3d.math.double._
import simplex3d.math.floatx.{ConstVec3f, Mat4f, ConstMat4f}
import simx.core.components.renderer.setup._
import simx.core.ontology._
import simx.core.ontology.referencesystems.CoordinateSystemConverter
import com.thoughtworks.xstream.XStream
import simx.core.ontology.types.OntologySymbol

import scala.xml.{XML, Node}

/**
 * Created by martin 
 * on 14/08/15.
 */

object Calibration {
  def apply(xmlFile: File): Calibration = {
    Symbols.vrpn
    val calibrationXml = XML.loadFile(xmlFile)
    val xStream = new XStream()
    val data = xStream.fromXML(calibrationXml.toString()).asInstanceOf[CalibrationData]
    val coordinateSystemName =
      OntologySymbol.lookup(Symbol(data.coordinateSystemName)).getOrElse(
        throw new Exception("Could not look up " + data.coordinateSystemName))
    new Calibration(
      data.screenTransformation,
      data.screenSize.toTuple,
      data.screenResolution.toTuple,
      coordinateSystemName,
      data.display
    )
  }
}

/**
 *
 * Created by martin on 12.08.15.
 */
class Calibration(
  screenTransformation: ConstMat4,
  screenSize: (Double, Double),
  screenResolution: (Int, Int) = (1920, 1080),
  coordinateSystemName: GroundedSymbol = Symbols.vrpn,
  display: Int = 0
) {

  val converter =
    new CoordinateSystemConverter[ConstMat4](coordinateSystemName, screenTransformation)

  def targetToScreenCoordinates(m : ConstMat4f) = ConstMat4f(converter.toRefCoords(m))

  def targetToScreenCoordinates(v : ConstVec3): ConstVec3f =
    ConstVec3f(converter.toRefCoords(ConstMat4f(Mat4x3.translate(v)))(3).xyz)

  def createDisplaySetupDescription(fullscreen : Boolean = false, stereo: Boolean = false): DisplaySetupDesc = {
    //val widthOfScreenInMeters: Double = widthInPx / dpi * 0.0254
    val transformation                = Mat4f.Identity
    val eyeSeparation                 = Some( 0.065f )
    val resolution                    = if (fullscreen) None else Some(screenResolution)
    val size                          = screenSize //widthOfScreenInMeters * heightInPx / widthInPx

    if(stereo) {
      val rightEyeDisplay = new DisplayDesc(resolution, size, transformation, new CamDesc(0, Eye.RightEye, eyeSeparation))
      val leftEyeDisplay = new DisplayDesc(resolution, size, transformation, new CamDesc(0, Eye.LeftEye, eyeSeparation))

      new DisplaySetupDesc().
        addDevice(new DisplayDevice(Some((display, 1, 0)), leftEyeDisplay :: Nil, LinkType.SingleDisplay), 0).
        addDevice(new DisplayDevice(Some((display, 0, 0)), rightEyeDisplay :: Nil, LinkType.SingleDisplay), 0)
    } else {
      val displayDesc = new DisplayDesc(resolution, size, transformation, new CamDesc(0, Eye.RightEye, Some( 0.0f )))
      new DisplaySetupDesc().addDevice(new DisplayDevice(Some((display, 0, 0)), displayDesc :: Nil, LinkType.SingleDisplay), 0)
    }
  }

  def saveTo(xmlFile: File): Unit = {
    val serializer = new XStream()
    val data = CalibrationData(coordinateSystemName.toString, Size(screenSize), Size(screenResolution), display, screenTransformation)
    val xml = scala.xml.Unparsed(serializer.toXML(data))
    save(xml, xmlFile, prettyPrint = true, addXmlDeclaration = true)
  }

  private def pretty(xml: Node) = {
    val prettyPrinter = new scala.xml.PrettyPrinter(80, 2)
    prettyPrinter.format(xml)
  }

  private def save(xml: Node, file: File, prettyPrint: Boolean = false, addXmlDeclaration: Boolean = false): Unit = {
    var xmlString = if(prettyPrint) pretty(xml) else xml.toString()
    if(addXmlDeclaration)
      xmlString = """<?xml version='1.0' encoding='UTF-8'?>""" + "\n" + xmlString

    val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), "UTF-8"))
    try {out.write(xmlString)} finally {out.close()}
  }
}

private case class CalibrationData(
  coordinateSystemName: String,
  screenSize: Size[Double],
  screenResolution: Size[Int],
  display: Int = 0,
  screenTransformation: ConstMat4
)

private object Size {
  def apply[T](size: (T, T)) = new Size(size._1, size._2)
}

private class Size[T](val width: T, val height: T) {
  def toTuple = (width, height)
}