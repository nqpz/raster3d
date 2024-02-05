import "../lib/github.com/athas/matte/colour"
import "../lib/github.com/diku-dk/cpprandom/random"
import "../types"
import "../transformations"
import "../hsv"

module rnge = xorshift128plus
module dist = uniform_real_distribution f32 rnge

def vec3_same f = {x=f, y=f, z=f}
def vec3_one = vec3_same vec3.one

module transformations = {
  def rotate' angle origo = map (rotate_triangle angle origo)
  def rotate angle = rotate' angle vec3.zero
  def translate offset = map (translate_triangle offset)
  def scale s = map (scale_triangle s)
}

open transformations

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
    let r = rectangle |> translate {x=0, y=0, z=0.5}
    in flatten [ r
               , r |> rotate (vec3.zero with y = f32.pi)
               , r |> rotate (vec3.zero with y = f32.pi * 0.5)
               , r |> rotate (vec3.zero with y = f32.pi * 1.5)
               , r |> rotate (vec3.zero with x = f32.pi * 0.5)
               , r |> rotate (vec3.zero with x = f32.pi * 1.5)
               ]
}

def generate (_seed: i32): (([](triangle, argb.colour), (f32, f32)), f32) =
  let t = shapes.cube
          |> scale (vec3_same 1000)
          |> translate (vec3.zero with y = -0.5 with z = 2000)
          |> scale (vec3_one with y = 2)

  let triangles = flatten (flatten (tabulate_2d 10 10 (\i j -> translate {x=f32.i64 i * 2000, y=0, z=f32.i64 j * 2000} t)))

  let colors = map (const (argb.gray 0.6)) triangles

  let ys = flatten (map (\(p, q, r) -> [p.y, q.y, r.y]) triangles)
  let y_min = reduce f32.min f32.inf ys
  let y_max = reduce f32.max (-f32.inf) ys

  in ((zip triangles colors, (y_min, y_max)), 2 * 10**7)
