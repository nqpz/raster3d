import "../lib/github.com/athas/matte/colour"
import "../lib/github.com/diku-dk/cpprandom/random"
import "../types"
import "../transformations"
import "../hsv"

module rnge = xorshift128plus
module dist = uniform_real_distribution f32 rnge

def vec3_same f = {x=f, y=f, z=f}
def vec3_one = vec3_same vec3.one

module shapes = {
  def rectangle: [2]triangle =
    let p0 = {x= -0.5, y= -0.5, z=0}
    let p1 = {x=  0.5, y= -0.5, z=0}
    let p2 = {x= -0.5, y=  0.5, z=0}
    let p3 = {x=  0.5, y=  0.5, z=0}
    let t0 = (p0, p1, p2)
    let t1 = (p1, p2, p3)
    in [t0, t1]

  def cube: [6 * 2]triangle =
    let center = vec3.zero
    let r = rectangle
            |> map (translate_triangle {x=0, y=0, z=0.5})
    in flatten [ r
               , r |> map (rotate_triangle (vec3.zero with y = f32.pi) center)
               , r |> map (rotate_triangle (vec3.zero with y = f32.pi * 0.5) center)
               , r |> map (rotate_triangle (vec3.zero with y = f32.pi * 1.5) center)
               , r |> map (rotate_triangle (vec3.zero with x = f32.pi * 0.5) center)
               , r |> map (rotate_triangle (vec3.zero with x = f32.pi * 1.5) center)
               ]
}

def generate (_seed: i32): (([](triangle, argb.colour), (f32, f32)), f32) =
  let t = shapes.cube
          |> map (scale_triangle (vec3_same 1000))
          |> map (translate_triangle (vec3.zero with y = -0.5 with z = 2000))
          |> map (scale_triangle (vec3_one with y = 2))

  let triangles = flatten (flatten (tabulate_2d 10 10 (\i j -> map (translate_triangle {x=f32.i64 i * 2000, y=0, z=f32.i64 j * 2000}) t)))

  let colors = map (const (argb.gray 0.6)) triangles

  let ys = flatten (map (\(p, q, r) -> [p.y, q.y, r.y]) triangles)
  let y_min = reduce f32.min f32.inf ys
  let y_max = reduce f32.max (-f32.inf) ys

  in ((zip triangles colors, (y_min, y_max)), 2 * 10**7)
