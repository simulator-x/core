/*
 * Copyright 2014 The SIRIS Project
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

import com.thoughtworks.xstream.XStream
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import Vector3._
import simplex3d.math.floatx.ConstMat4f

import scala.xml.Node

/**
 * Created by martin on
 * 7/30/14.
 */
object Ray {

  def main(args: Array[String]) {
    //println(rotateOnto(Vec3.UnitX, Vec3.UnitY))
    val ray1 = Ray(Vec3(-7, 2, -3), Vec3(0, 1, 2))
    val ray2 = Ray(Vec3(-3, -3, 3), Vec3(1, 2, 1))
    println("result = " +  findCenter(ray1, ray2))
  }

  def from(xml: Node): Ray = {
    val xStream = new XStream()
    val loadedRay = xStream.fromXML(xml.toString()).asInstanceOf[Ray]
    Ray(loadedRay.origin, simplex3d.math.double.functions.normalize(loadedRay.direction))
  }
  def apply(origin: ConstVec3, direction: ConstVec3) = new Ray(origin, direction)

  def fromTo(origin: ConstVec3, intersectionPoint: ConstVec3) =
    new Ray(origin, normalize(intersectionPoint - origin))

  def findCenter(ray1 : Ray, ray2 : Ray) = {
    val params = inverse(ConstMat3(-ray1.direction, ray2.direction, cross(ray1.direction, ray2.direction))) *
      (ray1.origin - ray2.origin)
    println("error: " + length((ray1 * params.x * 0.5 + ray2 * params.y * 0.5) - (ray1 * params.x)))
    ray1 * params.x * 0.5 + ray2 * params.y * 0.5
  }

  def calculateIntersection(rays : Iterable[Ray]) : ConstVec3 = {
    val x = rays.flatMap( cr => rays.filterNot(_ == cr).map(cr -> _).map(tuple => findCenter(tuple._1, tuple._2)))
    centerOf(x.map(_.toConst))
  }
  
  def from(m: ConstMat4) = {
    new Ray(m(3).xyz, m(2).xyz)
  } 
}

class Ray(val origin: ConstVec3, val direction: ConstVec3) {
  override def toString: String =
    "Ray with origin " + vec2String(origin) + " and direction " + vec2String(direction)

  private def vec2String(in : ConstVec3) =
    "(%1.5f, %1.5f, %1.5f)" format (in.x, in.y, in.z)

  def *(x : Double) = origin + x * direction

  def originInGlobal(trackingData : ConstMat4) : ConstVec3 =
    //trackingData(3).xyz + functions.inverse(Mat3(trackingData)) * origin
    (trackingData * Vec4(origin,1)).xyz

  def directionInGlobal(trackingData : ConstMat4) : ConstVec3 =
    //functions.inverse(Mat3(trackingData)) * direction
    Mat3(trackingData) * direction

  def closestPointOnRay(to : ConstVec3) : ConstVec3 = {
    origin + direction * dot(to - origin, direction) / functions.length(direction)
  }

  def distanceTo(p : ConstVec3) : Float = {
    length(p - closestPointOnRay(p)).toFloat
  }

  def intersectionWithPlane(n: ConstVec3, d: Double): ConstVec3 = {
    val t = -(functions.dot(origin, functions.normalize(n)) + d) / functions.dot(normalize(direction), normalize(n))
    origin + normalize(direction) * t
  }

  def toMatrix: ConstMat4f = {
    val z = normalize(direction)
    val y_temp = if(z.x > 0.01f || z.y > 0.01f) z.yxz else z.xzy
    val x = normalize(cross(y_temp, z))
    val y = normalize(cross(x, z))
    val p = origin
    ConstMat4f(Vec4(x, 0), Vec4(y, 0), Vec4(z, 0), Vec4(p, 1))
  }
}
