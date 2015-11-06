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

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.{CountDownLatch, Callable, FutureTask}
import javafx.stage.FileChooser.ExtensionFilter
import javafx.stage.{DirectoryChooser, Stage}
import javax.swing.filechooser.FileFilter
import javax.swing.{ImageIcon, JOptionPane}

import scala.collection.IterableLike
import scala.swing.FileChooser
import scala.swing.FileChooser.SelectionMode

object JfxRunnable {
  private var initialized = false
  def init(): Unit = synchronized {
    if(!initialized) {
      val initBarrier = new CountDownLatch(1)
      val fxRunnable = new JfxRunnable(initBarrier)
      val thread = new Thread(fxRunnable)
      thread.start()
      initialized = true
      initBarrier.await()
      //println("Jfx Thread Initialized")
    }
  }
}

private object JfxAppHelper {
  private var initBarrier: Option[CountDownLatch] = None
  def launch(initBarrier: CountDownLatch): Unit = {
    JfxAppHelper.initBarrier = Some(initBarrier)
    javafx.application.Application.launch(classOf[JfxAppHelper])    
  }
}

private class JfxAppHelper() extends javafx.application.Application {
  override def start(primaryStage: Stage): Unit = {
    JfxAppHelper.initBarrier.foreach{_.countDown()}
  }
}

class JfxRunnable(initBarrier: CountDownLatch) extends Runnable {
  override def run() = {JfxAppHelper.launch(initBarrier)}
}

object JfxContext {
  def call[T](fxCode: => T): T = {
    val futureTask = new FutureTask(new Callable[T]() {
      override def call(): T = fxCode
    })
    javafx.application.Platform.runLater(futureTask)
    futureTask.get()
  }
}

object Jfx {
  JfxRunnable.init()

  def askForFolder(question: String): Option[File] = {
    Option(JfxContext.call{
      val chooser = new DirectoryChooser()
      chooser.setInitialDirectory(new File("."))
      chooser.setTitle(question)
      chooser.showDialog(null)
    })
  }

  def askForFile(question: String, extension: IterableLike[String, _] = None): Option[File] = {
    Option(JfxContext.call{
      val chooser = new javafx.stage.FileChooser()
      chooser.setInitialDirectory(new File("."))
      chooser.setTitle(question)
      extension.foreach{e =>
        chooser.getExtensionFilters.add(new ExtensionFilter("*." + e, "*." + e))
      }
      chooser.showOpenDialog(null)
    })
  }
}

/**
 * Created by martin 
 * on 17/06/15.
 */
object IO {
  
  def askForOptions(question: String, options: String*) = {
    (options.size - 1) - JOptionPane.showOptionDialog(
      null,
      question,
      "SimX Application Configuration",
      JOptionPane.YES_NO_CANCEL_OPTION,
      JOptionPane.QUESTION_MESSAGE,
      new ImageIcon(Splash.loadLogo.getScaledInstance(32,32,java.awt.Image.SCALE_SMOOTH)),
      options.reverse.toArray,
      options.head
    )
  }

  def askForOption(question: String) =
    1 == JOptionPane.showOptionDialog(
      null,
      question,
      "SimX Application Configuration",
      JOptionPane.YES_NO_OPTION,
      JOptionPane.QUESTION_MESSAGE,
      new ImageIcon(Splash.loadLogo.getScaledInstance(32,32,java.awt.Image.SCALE_SMOOTH)),
      Array("No", "Yes"),
      "Yes"
    )

  private val defaultFileFilter = new FileFilter {
    override def getDescription: String = "All files"
    override def accept(f: File): Boolean = true
  }

  def extensionFilter(extension: String) =  new FileFilter {
    override def getDescription: String = "*." + extension
    override def accept(f: File): Boolean = f.isDirectory || f.getAbsolutePath.endsWith("." + extension)
  }

  @deprecated("Use Jfx.askForFolder instead", "02-07-2015")
  def askForFolderJfx(question: String): Option[File] =
    Jfx.askForFolder(question)

  @deprecated("Use Jfx.askForFile instead", "02-07-2015")
  def askForFileJfx(question: String, extension: Option[String] = None): Option[File] =
    Jfx.askForFile(question, extension)

  lazy val chooser = new FileChooser(new File("."))

  def askFor(
    mode: SelectionMode.Value,
    fileFilter: FileFilter = defaultFileFilter
    )(question: String): Option[File] =
  {
    chooser.fileSelectionMode = mode
    chooser.title = question
    chooser.fileFilter = fileFilter
    val result = chooser.showOpenDialog(null)
    if (result == FileChooser.Result.Approve) Some(chooser.selectedFile)
    else None
  }

  def askForFolder(question: String): Option[File] = askFor(FileChooser.SelectionMode.DirectoriesOnly)(question)

  def askForFile(question: String, fileFilter: FileFilter = defaultFileFilter): Option[File] =
    askFor(FileChooser.SelectionMode.FilesOnly, fileFilter)(question)

  def split(f: File) = f.getName.split('.').toList match {
    case n :: e :: Nil => (n, Some(e))
    case n :: Nil => (n, None)
    case n :: n2 :: e :: Nil => (n+n2, Some(e)) // added case if there are more than one point - take last extension as extension
    case _ => throw new Exception("[error][simx.core.helper.IO] while parsing file " + f.getAbsolutePath)
  }

  def dateTimeFileFrom(f: File) = {
    val (name, extension) = split(f)
    new File(f.getParent, name + "-" + getDateTimeString + extension.map{"." + _}.getOrElse(""))
  }

  def removeExtension(f: File) = {
    val (name, extension) = split(f)
    new File(f.getParent, name)
  }

  def changeExtension(f: File, newExtension: String): File = {
    val base = removeExtension(f)
    new File(base.getAbsolutePath + "." + newExtension)
  }

  def changeExtension(newExtension: String)(f: File): File =
    changeExtension(f, newExtension)

  def getDateTimeString =
    new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(new Date())
}
