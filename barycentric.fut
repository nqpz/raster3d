import "raster_types"

let interpolate (bary: barycentric) (t: triangle_slopes) (f: slope_point -> f32): f32 =
  let bary_w = 1 - bary.u - bary.v
  in bary.u * f t.p + bary.v * f t.q + bary_w * f t.r
