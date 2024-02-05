import "../lib/github.com/athas/matte/colour"
import "../lib/github.com/diku-dk/cpprandom/random"
import "../types"
import "../hsv"
import "generator_utils"

module rnge = xorshift128plus
module dist = uniform_real_distribution f32 rnge

open transf

def generate (_seed: i32): (([](triangle, argb.colour), (f32, f32)), f32) =
  let base = 1000
  let t = shape.cube
          |> scale (vec3_same base)
          |> translate (vec3.zero with y = -0.5 with z = 2 * base)
          |> scale (vec3_one with y = 2)

  let triangles =
    tabulate_2d 10 10 (\i j -> translate {x=f32.i64 i * 2 * base, y=0, z=f32.i64 j * 2 * base} t)
    |> flatten
    |> flatten

  let colors = map (const (argb.gray 0.6)) triangles

  let ys = flatten (map (\(p, q, r) -> [p.y, q.y, r.y]) triangles)
  let y_min = reduce f32.min f32.inf ys
  let y_max = reduce f32.max (-f32.inf) ys

  in ((zip triangles colors, (y_min, y_max)), 2 * 10**7)
