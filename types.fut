import "lib/github.com/athas/vector/vspace"
import "lib/github.com/athas/matte/colour"
import "quaternion"
import "quaternion_euler"

module vec3 = mk_vspace_3d f32
module quaternion = mk_quaternion f32
module qe_conversions = mk_quaternion_euler_conversions f32

type camera_base 'orientation = {position: vec3.vector, orientation: orientation}
type camera = camera_base vec3.vector -- Euler angles
type camera_quaternion = camera_base quaternion.quaternion
