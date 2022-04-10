import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/cpprandom/random"
import "raster_types"
import "hsv"

module rnge = xorshift128plus
module dist = uniform_real_distribution f32 rnge


def hsv_average ((h0, s0, v0): hsv) ((h1, s1, v1): hsv): hsv =
  let (h0, h1) = if h0 < h1 then (h0, h1) else (h1, h0)
  let diff_a = h1 - h0
  let diff_b = h0 + 360.0 - h1
  let h = if diff_a < diff_b
          then h0 + diff_a / 2.0
          else fmod (h1 + diff_b / 2.0) 360.0
  let s = (s0 + s1) / 2.0
  let v = (v0 + v1) / 2.0
  in (h, s, v)


def mix (c1: argb.colour) (c2: argb.colour): argb.colour =
  let (r1,g1,b1,a1) = argb.to_rgba c1
  let (r2,g2,b2,a2) = argb.to_rgba c2

  in argb.from_rgba ((r1 + r2) / 2)
                    ((g1 + g2) / 2)
                    ((b1 + b2) / 2)
                    ((a1 + a2) / 2)

def mix8 (a: hsv) (b: hsv) (c: hsv) (d: hsv) (e: hsv) (f: hsv) (g: hsv) (h: hsv): hsv =
  let m = hsv_average
  in m (m (m a b) (m c d)) (m (m e f) (m g h))

-- | A very simple terrain generator.
def generate_terrain
  (depth: i64)
  (width: i64)
  (size: i64)
  (fluct: f32)
  (smooth_iterations_areas: i32)
  (smooth_iterations_colours: i32)
  (seed: i32): ([](triangle, argb.colour), (f32, f32)) =

  -- Generate points.
  let size_vert = f32.i64 size / 2 ** 0.5
  let points =
    map (\(i: i64) ->
           let indent = (i % 2) * (size / 2)
           in map (\(j: i64) ->
                     {x=f32.i64 (j * size + indent),
                      y=0,
                      z=f32.i64 i * size_vert}
                  ) (0..<width)
        ) (0..<depth)

  -- Make random spikes.
  let rngs = rnge.rng_from_seed [seed]
             |> rnge.split_rng (depth * width)
             |> unflatten depth width
  let (rngs, points') =
    map2 (map2 (\rng (p: vec3.vector) ->
                  let (rng, y') = dist.rand (-fluct / 2, fluct / 2) rng
                  in (rng, p with y = y')
               )) rngs points |> map unzip |> unzip
  let rng = rnge.join_rng (flatten rngs)

  -- Smooth areas with a stencil.
  let points'' =
    iterate smooth_iterations_areas (\ps ->
      tabulate_2d depth width
                  (\i j ->
                     let p: vec3.vector = ps[i,j]
                     in if i >= 1 && i < depth - 1 && j >= 1 && j < width - 1
                        then p with y = (
                          ps[i-1,j-1].y + ps[i-1,j].y + ps[i-1,j+1].y +
                          ps[i,  j-1].y +               ps[i,  j+1].y +
                          ps[i+1,j-1].y + ps[i+1,j].y + ps[i+1,j+1].y) / 8
                        else p with y = 0
                  )) points'

  -- Make triangles.
  let triangles =
    let m = depth - 1
    let n = width - 1
    let n2 = n * 2
    in map3 (\row0 row1 i ->
               let (row0', row1') = if i % 2 == 0
                                    then (row0, row1)
                                    else (row1, row0)
               in flatten (map4 (\c0 c0t c1 c1t ->
                                   let tri0 = (c0, c0t, c1)
                                   let tri1 = (c1, c1t, c0t)
                                   in [tri0, tri1]
                                ) (row0'[:width-1] :> [n]vec3.vector) (row0'[1:] :> [n]vec3.vector) (row1'[:width-1] :> [n]vec3.vector) (row1'[1:] :> [n]vec3.vector)) :> [n2]triangle) (points''[:depth-1] :> [m][width]vec3.vector) (points''[1:] :> [m][width]vec3.vector) ((0..<depth-1) :> [m]i64)

  -- Colour triangles.
  let max_y = reduce f32.max (-fluct) (flatten triangles |> map (.0.y))
  let min_y = reduce f32.min fluct (flatten triangles |> map (.0.y))

  let (n_triangles0, n_triangles1) = (depth - 1, 2 * (width - 1))
  let triangles = triangles :> [n_triangles0][n_triangles1]triangle
  let rngs = rng
             |> rnge.split_rng (n_triangles0 * n_triangles1)
             |> unflatten n_triangles0 n_triangles1
  let triangles_coloured =
    map2 (map2 (\rng t ->
                  let h = 360.0 * ((t.0.y - min_y) / (max_y - min_y))
                  let (rng, h') = dist.rand (h - 30.0, h + 30.0) rng
                  let (rng, s) = dist.rand (0.5, 1.0) rng
                  let (_rng, v) = dist.rand (0.5, 1.0) rng
                  in (t, (h', s, v))
               )) rngs triangles

  -- Smooth the colours.
  let triangles_coloured' =
    iterate smooth_iterations_colours (\(tc: [n_triangles0][n_triangles1](triangle, hsv)) ->
      tabulate_2d n_triangles0 n_triangles1
                  (\i j ->
                     let t = tc[i,j]
                     in if i >= 1 && i < n_triangles0 - 1 && j >= 1 && j < n_triangles1 - 1
                        then t with 1 = (mix8
                          tc[i-1,j-1].1 tc[i-1,j].1 tc[i-1,j+1].1
                          tc[i,  j-1].1             tc[i,  j+1].1
                          tc[i+1,j-1].1 tc[i+1,j].1 tc[i+1,j+1].1)
                        else t
                  )) triangles_coloured

  let triangles_coloured'' = map (map (\t ->
                                         (t.0, hsv_to_rgb t.1)
                                      )) triangles_coloured'

  let (triangles, colors) = unzip (flatten triangles_coloured'')
  let ys = flatten (map (\(p, q, r) -> [p.y, q.y, r.y]) triangles)
  let y_min = reduce f32.min f32.inf ys
  let y_max = reduce f32.max (-f32.inf) ys
  in (zip triangles colors, (y_min, y_max))

-- ==
-- entry: benchmark
-- compiled input { 1000i64 1000i64 300i64 100000f32 64 3 0 }
entry benchmark
  (depth: i64)
  (width: i64)
  (size: i64)
  (fluct: f32)
  (smooth_iterations_areas: i32)
  (smooth_iterations_colours: i32)
  (seed: i32): ([]triangle, []argb.colour, f32, f32) =
  let (a, b) = generate_terrain depth width size fluct smooth_iterations_areas smooth_iterations_colours seed
  let (a0, a1) = unzip a
  in (a0, a1, b.0, b.1)
