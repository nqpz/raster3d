import "../lib/github.com/athas/matte/colour"
import "../lib/github.com/diku-dk/cpprandom/random"
import "../types"
import "generator_utils"

module rnge = xorshift128plus
module dist = uniform_real_distribution f32 rnge

open transf

def generate (_pos: vec3.vector) (seed: i32): (([](triangle, argb.colour), (f32, f32)), f32) =
  let base = 1000
  let t = shape.cube
          |> scale (vec3_same base)
          |> translate (vec3.zero with y = -base / 2 with z = 2 * base)

  let main_rng = rnge.rng_from_seed [seed]

  let triangles_coloured =
    tabulate_2d 100 100 (\i j ->
                           let rng = rnge.join_rng [ rnge.rng_from_seed [i32.i64 i]
                                                   , rnge.rng_from_seed [i32.i64 j]
                                                   , main_rng ]
                           let (rng, hf) = dist.rand (2, 5) rng
                           let (_rng, gf) = dist.rand (0.2, 0.8) rng
                           let ts = copy t
                                    |> scale (vec3_one with y = hf)
                                    |> translate {x=f32.i64 i * 2 * base, y=0, z=f32.i64 j * 2 * base}
                           let colors = map (const (argb.gray gf)) ts
                           in zip ts colors)
    |> flatten
    |> flatten

  let ys = flatten (map (\((p, q, r), _) -> [p.y, q.y, r.y]) triangles_coloured)
  let y_min = reduce f32.min f32.inf ys
  let y_max = reduce f32.max (-f32.inf) ys

  in ((triangles_coloured, (y_min, y_max)), 2 * 10**7)
