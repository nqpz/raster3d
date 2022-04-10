import "lib/github.com/athas/vector/vspace"
import "lib/github.com/athas/matte/colour"

module vec3 = mk_vspace_3d f32

type point_2d = {x: i32, y: i32}

type barycentric = {u: f32, v: f32} -- w can be calculated in terms of u and v

type base_component 'extra = {extra: extra, bary: barycentric}

type point_projected = {projected: point_2d, z: f32, world: vec3.vector}
type point_projected_with_bary = base_component {projected: point_2d, z: f32, world: vec3.vector}

type point_projected_final = base_component point_2d

type triangle = (vec3.vector, vec3.vector, vec3.vector)
type triangle_projected =
  (point_projected, point_projected, point_projected)
type triangle_projected_with_bary =
  (point_projected_with_bary, point_projected_with_bary, point_projected_with_bary)

type slope_point = base_component {x: i32, z_inv: f32, world: vec3.vector}
type slope = base_component {x: f32}
type triangle_slopes = {n_lines: i32,
                        y: i32,
                        y_subtracted_p_y: {q: i32, r: i32},
                        p: slope_point,
                        r: slope_point,
                        q: slope_point,
                        s1: slope,
                        s2: slope,
                        s3: slope}

type line_component = base_component {x: i32}
type line = {n_points: i32,
             y: i32,
             leftmost: line_component,
             step: line_component}

type pixel_final = base_component {i: i32, z: f32}

type^ pixel_color_function 'a = (pixel_final, a) -> argb.colour

type trig = {sin: vec3.vector, cos: vec3.vector}
