import "lib/github.com/athas/matte/colour"
import "raster_types"
import "barycentric"
import "hsv"

module pixel_color = {
  def view_dist = 0f32
  def pixel_depth (draw_dist: f32) (z: f32): f32 =
    if z < -view_dist
    then 1
    else (z + view_dist) / (draw_dist + view_dist)

  module by_triangle = {
    type aux = argb.colour
    def empty_aux = argb.white
    def triangles_aux colors = colors
    def pixel_color (view_dist: f32) (flashlight_brightness: f32) ((p, color): (pixel_final, argb.colour)): argb.colour =
      let v_factor = f32.min 1.0 (flashlight_brightness / ((p.extra.z + view_dist) ** 2.0))
      in argb.mix v_factor color (1 - v_factor) argb.black
  }

  module by_depth = {
    type aux = ()
    def empty_aux = ()
    def triangles_aux [n] (_: [n]triangle_slopes): [n]() = replicate n ()
    def pixel_color (draw_dist: f32) ((p, _aux): (pixel_final, ())): argb.colour =
      argb.gray (pixel_depth draw_dist p.extra.z)
  }

  module by_height = {
    type aux = triangle_slopes
    def empty_aux = ()
    def triangles_aux [n] (_: [n]triangle_slopes): [n]() = replicate n ()
    def pixel_color (y_min: f32) (y_span: f32) (draw_dist: f32) (ts: []triangle_slopes) ((p, _aux): (pixel_final, ())): argb.colour =
      if p.extra.z == f32.inf
      then argb.white
      else let h = if p.extra.i == -1
                   then 0
                   else let t = ts[p.extra.i]
                        -- FIXME: This interpolation is affine and produces
                        -- slightly wrong y values (think PlayStation 1
                        -- graphics).
                        let world_y = interpolate p.bary t (.extra.world.y)
                        let f = (world_y - y_min) / y_span
                        in 360 * f
           in hsv_to_rgb (h, 1 - pixel_depth draw_dist p.extra.z, 0.5)
  }
}
