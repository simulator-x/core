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

import javax.swing._
import java.awt.{BorderLayout, Font, Toolkit, Color}
import java.net.URL
import javax.imageio.ImageIO

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 12/6/12
 * Time: 4:13 PM
 */
object Splash {
  def apply(scale: Float = 1f) = new Splash(scale)
}

class Splash private (scale: Float = 1f) {
  private lazy val _splash = new JWindow {
    getContentPane.setBackground(Color.white)

    val _width = (231.2f * scale).toInt
    val _height = (219.6f * scale).toInt
    val screen = Toolkit.getDefaultToolkit.getScreenSize
    val _x = (screen.width - _width) / 2
    val _y = (screen.height - _height) / 2
    setBounds(_x,_y,_width,_height)

    val url: URL = getClass.getResource("/simx/core/assets/simx-logo.png")
    val label = new JLabel(
      new ImageIcon(ImageIO.read(url).getScaledInstance(_width, _height,java.awt.Image.SCALE_SMOOTH)))
    val copyright = new JLabel("Copyright 2013 The SIRIS Project", SwingConstants.CENTER)
    copyright.setFont(new Font("Sans-Serif", Font.PLAIN, 14))
    getContentPane.add(label, BorderLayout.CENTER)
    getContentPane.add(copyright, BorderLayout.SOUTH)
  }

  def show() = {_splash.setVisible(true); _splash.repaint(); this}
  def dispose() {_splash.dispose()}
}
