package simx.core.helper

import simplex3d.math.double._
import simplex3d.math.doublex.functions._

/**
 * Created by martin 
 * on 30/07/15.
 */
object Matrix {
  def getRotationAxisAndAngle(m1: ConstMat4, m2 : ConstMat4) : (ConstVec3, Double) = {
    var quat = diffRot(m1, m2)
    if (quat.a > 1f)
      quat = functions.normalize(quat) // if w>1 acos and sqrt will produce errors, this cant happen if quaternion is normalised
    val angle = 2.0 * Math.acos(quat.a)
    val s = Math.sqrt(1.0 - quat.a*quat.a) // assuming quaternion normalised then w is less than 1, so term always positive.
    if (s < 0.00001)  // test to avoid divide by zero, s is always positive due to sqrt
    // if s close to zero then direction of axis not important
      ConstVec3(quat.b, quat.c, quat.d) -> angle// if it is important that axis is normalised then replace with x=1; y=z=0;
    else
      (Vec3(quat.b, quat.c, quat.d) / s).toConst -> angle
  }

  def diffRot(m1 : ConstMat4, m2 : ConstMat4) : Quat4 =
    functions.quaternion(ConstMat3(functions.inverse(m1) * m2))

  def rotationMat(from : ConstVec3, to : ConstVec3) : ConstMat3 = {
    val rotAxis = normalize(cross(to, from))
    val angle = acos(dot(normalize(to), normalize(from)))
    simplex3d.math.doublex.functions.rotationMat(angle, rotAxis)
  }
}
