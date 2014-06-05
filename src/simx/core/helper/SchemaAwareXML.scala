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

/*
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 4/7/11
 * Time: 9:27 AM
 */
package simx.core.helper

import javax.xml.parsers.SAXParser
import javax.xml.parsers.SAXParserFactory
import org.xml.sax.InputSource
import xml.parsing.NoBindingFactoryAdapter
import xml.{TopScope, Elem}
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.{SchemaFactory, Schema}
import javax.xml.XMLConstants
import java.io.{ByteArrayInputStream, FileInputStream, File}


/**
 * From "Sean Wellington's Blog"
 * Url "http://sean8223.blogspot.com/2009/09/xsd-validation-in-scala.html"
 */
class SchemaAwareFactoryAdapter(schema:Schema) extends NoBindingFactoryAdapter {

  def loadXML(source: InputSource): Elem = {
    // create parser
    val parser: SAXParser = try {
      val f = SAXParserFactory.newInstance()
      f.setNamespaceAware(true)
      f.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
      f.newSAXParser()
    } catch {
      case e: Exception =>
        Console.err.println("error: Unable to instantiate parser")
        throw e
    }

    val xr = parser.getXMLReader
    val vh = schema.newValidatorHandler()
    vh.setContentHandler(this)
    xr.setContentHandler(vh)

    // parse file
    scopeStack.push(TopScope)
    xr.parse(source)
    scopeStack.pop()
    rootElem.asInstanceOf[Elem]
  }

  def loadXML(f: File): Elem =
    loadXML(new InputSource(new FileInputStream(f)))
}

object SchemaAwareXML {

  def loadFile(f: File): Elem = {
    val root = xml.XML.loadFile(f)
    val xsd = root.attributes.find(_.key == "noNamespaceSchemaLocation").
      orElse(root.attributes.find(_.key == "schemaLocation"))

    if(!xsd.isDefined)
      root
    else {
      var xsdFile = new File(xsd.get.value.toString())
      if (!xsdFile.exists) xsdFile = new File(f.getParent, xsd.get.value.toString())

      val sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
      val s = sf.newSchema(new StreamSource(xsdFile))
      val is = new InputSource(new FileInputStream(f))
      new SchemaAwareFactoryAdapter(s).loadXML(is)
      root
    }
  }

  private lazy val sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)

  def loadFile(f: File, schema: String) = (new SchemaAwareFactoryAdapter(
    sf.newSchema(new StreamSource(new ByteArrayInputStream(schema.getBytes))))).loadXML(f)

}