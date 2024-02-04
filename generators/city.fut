import "../lib/github.com/athas/matte/colour"
import "../lib/github.com/diku-dk/cpprandom/random"
import "../types"
import "../transformations"
import "../hsv"

module rnge = xorshift128plus
module dist = uniform_real_distribution f32 rnge

module shapes = {
  def rectangle (x0: f32) (y0: f32) (w: f32) (h: f32): [2]triangle =
    let p0 = {x=x0, y=y0, z=0}
    let p1 = {x=x0 + w, y=y0, z=0}
    let p2 = {x=x0, y=y0 + h, z=0}
    let p3 = {x=x0 + w, y=y0 + h, z=0}
    let t0 = (p0, p1, p2)
    let t1 = (p1, p2, p3)
    in [t0, t1]

  def cube (center: vec3.vector) (size: f32): [6 * 2]triangle =
    let radius = size / 2
    let r = rectangle (center.x - radius) (center.y - radius) size size
            |> map (translate_triangle {x=0, y=0, z=center.z + radius})
    in flatten [ r
               , r |> map (rotate_triangle (vec3.zero with y = f32.pi) center)
               , r |> map (rotate_triangle (vec3.zero with y = f32.pi * 0.5) center)
               , r |> map (rotate_triangle (vec3.zero with y = f32.pi * 1.5) center)
               , r |> map (rotate_triangle (vec3.zero with x = f32.pi * 0.5) center)
               , r |> map (rotate_triangle (vec3.zero with x = f32.pi * 1.5) center)
               ]
}

def generate (_seed: i32): ([](triangle, argb.colour), (f32, f32)) =
  let triangles = shapes.cube (vec3.zero with z = 500) 250

  let colors = map (const (argb.gray 0.6)) triangles

  let ys = flatten (map (\(p, q, r) -> [p.y, q.y, r.y]) triangles)
  let y_min = reduce f32.min f32.inf ys
  let y_max = reduce f32.max (-f32.inf) ys

  in (zip triangles colors, (y_min, y_max))
