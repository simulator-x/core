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

/**
 * Created by
 * martin
 * in September 2015.
 */
abstract class SpeechRecognitionResult {
  def text: String
  def confidence: Float
  def timestamp: Long
}

case class Hypothesized(text: String, confidence: Float, timestamp: Long) extends SpeechRecognitionResult
case class Recognized(text: String, confidence: Float, timestamp: Long) extends SpeechRecognitionResult
case class Silence(timestamp: Long = System.currentTimeMillis()) extends SpeechRecognitionResult {
  def text = ""
  def confidence = 1f
}