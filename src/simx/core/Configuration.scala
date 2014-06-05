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

package simx.core

import helper.{JVMTools, SchemaAwareXML}
import java.io.{FileNotFoundException, File}
import xml.{Group, Node}
import org.xml.sax.SAXException
import org.slf4j.LoggerFactory

trait SimXConfig {
  Configuration.load()
}

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 11/19/12
 * Time: 3:32 PM
 */
object Configuration {

  private val log = LoggerFactory.getLogger(getClass)
  private var loaded = false

  case class Component(path: File)

  val configFile = new File("simxConfig.xml")

  lazy val configXML: Node = try {
    SchemaAwareXML.loadFile(configFile, configSchema)
  } catch {
    case e: SAXException =>
      Console.err.println("[SimXConfig] error while parsing " + configFile + ": " + e)
      Console.err.println("[SimXConfig] the " + configFile +
        " is probably not valid. Look at the corresponding schema or documentation.")
      new Group(Seq[Node]())
    case e: FileNotFoundException =>
      Console.err.println("[SimXConfig] used but no '" + configFile.getName + "' found in working directory (" +
        new File("./").getAbsoluteFile + ")")
      new Group(Seq[Node]())
    case t : Throwable =>
      t.printStackTrace()
      Console.err.println("[SimXConfig] unknown error while parsing " + configFile)
      new Group(Seq[Node]())
  }

  lazy val components = (configXML \\ "Component").map(n =>
    Component(new File((n \ "Path").text))).toList

  lazy val scalaHome = (configXML \\ "ScalaHome").headOption.collect{case sh => new File((sh \ "Path").text)}

  private def addNativePathsToJVM() {
    var nativePaths = Map[Os, List[File]]()
    def addOs(osName: String, nativePath: File) {
      nativePaths = nativePaths.updated(Os(osName), nativePath :: nativePaths.getOrElse(Os(osName), List[File]()) )
    }
    components.foreach(comp => {
      if(comp.path.exists()) {
        val nativesXML = getNativesXML(comp.path)
        Os.supportedOsNames.foreach(osName => {
          (nativesXML \\ osName).headOption.collect{case os => addOs(osName, new File(comp.path, (os \ "Path").text))}
        })
      }
    })
    JVMTools.addNatives(nativePaths)
  }

  private def addOtherJVMOptions() {
    scalaHome.collect{case sh => JVMTools.setProperty("scala.home", sh)}
  }

  def load() {
    if(!loaded) {
      addNativePathsToJVM()
      addOtherJVMOptions()
      loaded = true
    }
  }

  private def getNativesXML(componentPath: File) = try {
    SchemaAwareXML.loadFile(new File(componentPath, "natives.xml"), nativesSchema)
  } catch {
    case e: SAXException =>
      log.warn("Error while parsing " + (new File(componentPath, "natives.xml") + ": " + e))
      log.warn("The " + new File(componentPath, "natives.xml")
        + " is probably not valid. Look at the corresponding schema or documentation.")
      new Group(Seq[Node]())
    case _ : Throwable =>
      log.warn("Unknown error while parsing " + new File(componentPath, "natives.xml"))
      new Group(Seq[Node]())
  }

  val configSchema =
    """<?xml version="1.0"?>
      |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
      |
      |<xs:element name="SimxConfig" type="SimxConfigType"/>
      |
      |<xs:complexType name="SimxConfigType">
      |	<xs:sequence>
      |   <xs:element name="ScalaHome" type="pathType"  minOccurs="0" maxOccurs="1"/>
      |		<xs:element name="Components" type="componentsType"  minOccurs="0" maxOccurs="unbounded"/>
      |	</xs:sequence>
      |</xs:complexType>
      |
      |<xs:complexType name="componentsType">
      |	<xs:sequence>
      |		<xs:element name="Component" type="pathType" minOccurs="0" maxOccurs="unbounded"/>
      |	</xs:sequence>
      |</xs:complexType>
      |
      |<xs:complexType name="pathType">
      |	<xs:all>
      |		<xs:element name="Path" type="xs:string"/>
      |	</xs:all>
      |</xs:complexType>
      |
      |</xs:schema>
    """.stripMargin

  object Os {
    val supportedOsNames = "Linux32" :: "Linux64" :: "MacOSX" :: "Win32" :: "Win64" :: Nil

    def apply(osName: String): Os = osName match {
      case "Linux32" => Linux32()
      case "Linux64" => Linux64()
      case "MacOSX" => MacOSX()
      case "Win32" => Win32()
      case "Win64" => Win64()
      case _ => throw new Exception("Unsupported operating system.")
    }
  }

  abstract class Os()
  case class Linux32() extends Os()
  case class Linux64() extends Os()
  case class MacOSX() extends Os()
  case class Win32() extends Os()
  case class Win64() extends Os()

  val nativesSchema =
    """<?xml version="1.0"?>
      |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
      |
      |<xs:element name="NativeLibraries" type="NativeLibrariesType"/>
      |
      |<xs:complexType name="NativeLibrariesType">
      |	<xs:all>
      |		<xs:element name="Linux32" type="OSType" minOccurs="0" maxOccurs="1"/>
      |		<xs:element name="Linux64" type="OSType" minOccurs="0" maxOccurs="1"/>
      |		<xs:element name="MacOSX" type="OSType" minOccurs="0" maxOccurs="1"/>
      |		<xs:element name="Win32" type="OSType" minOccurs="0" maxOccurs="1"/>
      |		<xs:element name="Win64" type="OSType" minOccurs="0" maxOccurs="1"/>
      |	</xs:all>
      |</xs:complexType>
      |
      |<xs:complexType name="OSType">
      |	<xs:all>
      |		<xs:element name="Path" type="xs:string"/>
      |	</xs:all>
      |</xs:complexType>
      |
      |</xs:schema>
    """.stripMargin

}
