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

import simplex3d.math.ConstVec2i
import java.util.UUID
import java.awt.image.{DataBufferByte, BufferedImage}
import java.awt.Color

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
    new TextureData(ConstVec2i(img.getWidth, img.getHeight), img.getData.getDataBuffer.asInstanceOf[DataBufferByte].getData)

  def apply(c : Color) : TextureData = {
    val data = Array[Byte](c.getRed.toByte, c.getGreen.toByte, c.getBlue.toByte, c.getAlpha.toByte)
    TextureData(ConstVec2i(1,1), data)
  }
}

class TextureData(val size: ConstVec2i, val data: Array[Byte]) extends Serializable{
  val id = UUID.randomUUID()

  override def toString: String =
    "Texture-"+ id + " (" + size.x + "x"+ size.y +", "+ data.size +" bytes)"
}
