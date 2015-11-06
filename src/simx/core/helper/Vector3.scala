package simx.core.helper

import simplex3d.math.double._

/**
 * Created by martin 
 * on 30/07/15.
 */
object Vector3 {
  def centerOf(vectors : Iterable[ConstVec3]) : ConstVec3 =
    vectors.foldLeft(Vec3.Zero)(_ + _) / vectors.size
}
