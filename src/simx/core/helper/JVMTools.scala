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

package simx.core.helper

import java.io.{IOException, File}
import simx.core.Configuration._
import simx.core.Configuration.MacOSX
import simx.core.Configuration.Win32
import simx.core.Configuration.Win64

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 11/16/12
 * Time: 4:33 PM
 */
object JVMTools {

  def addNatives(osPaths: Map[Os, List[File]]) {
    osPaths.get(os).collect{case paths => paths.foreach(addToLibraryPath)}
  }

  def addToLibraryPath(newPath: String) {
    try {
        // This enables the java.library.path to be modified at runtime
        // From a Sun engineer at http://forums.sun.com/thread.jspa?threadID=707176
        val field = classOf[ClassLoader].getDeclaredField("usr_paths")
        field.setAccessible(true)
        val paths = field.get(null).asInstanceOf[Array[String]].toList
        if(paths.contains(newPath)) return
        field.set(null, (newPath :: paths).toArray)
        System.setProperty(
          "java.library.path",
          System.getProperty("java.library.path") + File.pathSeparator + newPath)
    } catch {
      case _ : IllegalAccessException =>
        throw new IOException("Failed to get permissions to set library path")
      case _ : NoSuchFieldException =>
        throw new IOException("Failed to get field handle to set library path")
    }
  }

  def setProperty(key: String, value: String) {System.setProperty(key,value)}

  def setProperty(key: String, value: File) {if(value.exists()) setProperty(key, value.getAbsolutePath)}

  def addToLibraryPath(newPath: File) {
    if(newPath.exists()) addToLibraryPath(newPath.getAbsolutePath)
  }

  private def os: Os =
    if(isMac) MacOSX()
    else if(isWindows) if (is64) Win64() else Win32()
    else if(isUnix) if (is64) Linux64() else Linux32()
    else throw new Exception("Unsupported operating system.")

  def isWindows = is("win")

  def isMac = is("mac")

  def isUnix = is("nix") ||  is("nux")

  def is64 =
    System.getProperty("os.arch").toLowerCase.indexOf("64") >= 0

  private def is(pattern: String) =
    System.getProperty("os.name").toLowerCase.indexOf(pattern) >= 0
}
