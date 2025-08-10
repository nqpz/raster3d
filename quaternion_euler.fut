-- | Conversions between quaternions and Euler angles.

-- Based on
-- http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/index.htm

import "lib/github.com/athas/vector/vspace"
import "quaternion"

-- x corresponds to roll/bank
-- y corresponds to pitch/heading
-- z corresponds to yaw/attitude

module mk_quaternion_euler_conversions (real: real) = {
  module vec3 = mk_vspace_3d real
  module quaternion = mk_quaternion real

  def euler_to_quaternion ({x, y, z}: vec3.vector): quaternion.quaternion =
    let (x', y', z') = real.((x / i32 2, y / i32 2, z / i32 2))
    let c1 = real.cos y'
    let s1 = real.sin y'
    let c2 = real.cos z'
    let s2 = real.sin z'
    let c3 = real.cos x'
    let s3 = real.sin x'
    let a = real.(c1 * c2 * c3 - s1 * s2 * s3)
    let b = real.(s1 * s2 * c3 + c1 * c2 * s3)
    let c = real.(s1 * c2 * c3 + c1 * s2 * s3)
    let d = real.(c1 * s2 * c3 - s1 * c2 * s3)
    in quaternion.mk a b c d

  def quaternion_to_euler (q: quaternion.quaternion): vec3.vector =
    let sqa = real.(q.a * q.a)
    let sqb = real.(q.b * q.b)
    let sqc = real.(q.c * q.c)
    let sqd = real.(q.d * q.d)
    let unit = real.(sqa + sqb + sqc + sqd)
    let test = real.(q.b * q.c + q.d * q.a)
    in if real.(test > f32 0.499 * unit) -- singularity at north pole
       then {x=real.i32 0, y=real.(i32 2 * atan2 q.b q.a), z=real.(pi / i32 2)}
       else if real.(test < f32 (-0.499) * unit) -- singularity at south pole
       then {x=real.i32 0, y=real.(i32 (-2) * atan2 q.b q.a), z=real.(pi / i32 (-2))}
       else {x=real.(atan2 (i32 2 * q.b * q.a - i32 2 * q.c * q.d) (i32 0 - sqb + sqc - sqd + sqa)),
             y=real.(atan2 (i32 2 * q.c * q.a - i32 2 * q.b * q.d) (sqb - sqc - sqd + sqa)),
           z=real.(asin (i32 2 * test / unit))}
}
