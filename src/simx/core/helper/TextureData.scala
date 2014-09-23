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

import simplex3d.math.{Vec2i, ConstVec2i}
import java.util.UUID
import java.awt.image._
import java.awt.{Transparency, Color}
import java.awt.color.ColorSpace
import scala.Some

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 5/30/13
 * Time: 9:13 PM
 */

object TextureData{
  def apply(size: ConstVec2i, data: Array[Byte]) =
    new TextureData(size, data)

  def apply(img : BufferedImage) =
    new TextureData(ConstVec2i(img.getWidth, img.getHeight), imageToTextureData(img))

  def apply(c : Color) : TextureData = {
    val data = Array[Byte](c.getRed.toByte, c.getGreen.toByte, c.getBlue.toByte, c.getAlpha.toByte)
    TextureData(ConstVec2i(1,1), data)
  }

  def imageToTextureData(img: BufferedImage): Array[Byte] = {
    def swapArray(array: Array[Byte])(a: Int, b: Int) {val temp = array(a); array(a) = array(b); array(b) = temp}

    img.getType match {
      case BufferedImage.TYPE_4BYTE_ABGR =>
        val array = img.getData.getDataBuffer.asInstanceOf[DataBufferByte].getData
        def swap = swapArray(array) _
        //ABGR (java.awt) to RGBA (jvr.core.Texture2D)
        for(pixelData <- 0 until (img.getWidth * img.getHeight * 4) by 4){
          swap(pixelData, pixelData + 3) //alpha <-> red
          swap(pixelData + 1, pixelData + 2)//blue <-> green
        }
        array
      case BufferedImage.TYPE_3BYTE_BGR =>
        val array = img.getData.getDataBuffer.asInstanceOf[DataBufferByte].getData
        def swap = swapArray(array) _
        //BGR (java.awt) to RGB (jvr.core.Texture2D)
        for(pixelData <- 0 until (img.getWidth * img.getHeight * 3) by 3){
          swap(pixelData, pixelData + 2) //blue <-> red
        }
        array
      case _ => throw new Exception("[TextureData] Unsupported image type.")
    }
  }
}

class TextureData(val size: ConstVec2i, val data: Array[Byte]) extends Serializable{
  val id = UUID.randomUUID()

  override def toString: String =
    "Texture-"+ id + " (" + size.x + "x"+ size.y +", "+ data.size +" bytes)"

  private def createRGBImage = {
    if(data.length == size.x * size.y * 3) {
      val buffer = new DataBufferByte(data, data.length)
      val cm = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB), Array[Int](8, 8, 8), false, false, Transparency.OPAQUE, DataBuffer.TYPE_BYTE)
      new BufferedImage(cm, Raster.createInterleavedRaster(buffer, size.x, size.y, size.x * 3, 3, Array[Int](0, 1, 2), null), false, null)
    } else {
      val buffer = new DataBufferByte(data, data.length)
      val cm = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB), Array[Int](8, 8, 8, 8), true, false, Transparency.OPAQUE, DataBuffer.TYPE_BYTE)
      new BufferedImage(cm, Raster.createInterleavedRaster(buffer, size.x, size.y, size.x * 4, 4, Array[Int](0, 1, 2, 3), null), false, null)
    }
  }

  def toImage: Option[BufferedImage] = if(size != Vec2i.Zero) Some(createRGBImage) else None

}
