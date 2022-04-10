import "lib/github.com/athas/matte/colour"
import "types"
import "raster_types"
import "scanline"
import "barycentric"

def prepare_triangles [n] (triangles: [n]triangle_projected): [n]triangle_slopes =
  map normalize_triangle_points triangles
  |> map (\(triangle: triangle_projected) ->
            ({bary={u=1, v=0}, extra=triangle.0},
             {bary={u=0, v=1}, extra=triangle.1},
             {bary={u=0, v=0}, extra=triangle.2}))
  |> map triangle_slopes

def rotate_x ({sin, cos}: trig)
             ({x, y, z}: vec3.vector): vec3.vector =
  {x,
   y=y * cos.x - z * sin.x,
   z=y * sin.x + z * cos.x}

def rotate_y ({sin, cos}: trig)
             ({x, y, z}: vec3.vector): vec3.vector =
  {x=z * sin.y + x * cos.y,
   y,
   z=z * cos.y - x * sin.y}

def rotate_z ({sin, cos}: trig)
             ({x, y, z}: vec3.vector): vec3.vector =
  {x=x * cos.z - y * sin.z,
   y=x * sin.z + y * cos.z,
   z}

def rotations (angle: vec3.vector) =
  let trig = {sin={x=f32.sin angle.x, y=f32.sin angle.y, z=f32.sin angle.z},
              cos={x=f32.cos angle.x, y=f32.cos angle.y, z=f32.cos angle.z}}
  in {x=rotate_x trig, y=rotate_y trig, z=rotate_z trig}

def rotate_point_base (origo: vec3.vector)
                      (rotate: vec3.vector -> vec3.vector) (p: vec3.vector): vec3.vector =
  id {x=p.x - origo.x, y=p.y - origo.y, z=p.z - origo.z}
  |> rotate
  |> (origo vec3.+)

def rotate_point (angle: vec3.vector) (origo: vec3.vector)
                 (p: vec3.vector): vec3.vector =
  let r = rotations angle
  in rotate_point_base origo (r.y >-> r.z >-> r.x) p

def rotate_point_inv (angle: vec3.vector) (origo: vec3.vector)
                     (p: vec3.vector): vec3.vector =
  let r = rotations angle
  in rotate_point_base origo (r.x >-> r.z >-> r.y) p

-- | Translate and rotate all points relative to the camera.
def camera_normalize_triangle
    ({position=pc, orientation=po}: camera)
    ((p, q, r): triangle): triangle =

  let camera_normalize_point (p: vec3.vector): vec3.vector =
    vec3.(p - pc)
    |> rotate_point vec3.(zero - po) vec3.zero

  in (camera_normalize_point p,
      camera_normalize_point q,
      camera_normalize_point r)

def project_triangle
    (h_half: f32) (w_half: f32)
    (view_dist: f32)
    (camera: camera)
    (world: triangle): triangle_projected =

  let project_point ({x, y, z}: vec3.vector): point_2d =
    let z_ratio = if z >= 0.0
                  then (view_dist + z) / view_dist
                  else 1.0 / ((view_dist - z) / view_dist)
    let x_projected = x / z_ratio + w_half
    let y_projected = y / z_ratio + h_half
    in {x=t32 (f32.round x_projected), y=t32 (f32.round y_projected)}

  let normalized = camera_normalize_triangle camera world
  in ({projected=project_point normalized.0, world=world.0, z=normalized.0.z},
      {projected=project_point normalized.1, world=world.1, z=normalized.1.z},
      {projected=project_point normalized.2, world=world.2, z=normalized.2.z})

-- | Project triangles currently visible from the camera.
def project_triangles_in_view 'a
    (h: i64)
    (w: i64)
    (view_dist: f32)
    (draw_dist: f32)
    (camera: camera)
    (triangles_with_aux: [](triangle, a)): [](triangle_slopes, a) =

  let triangles = map (.0) triangles_with_aux
  let triangles_projected = map (project_triangle (f32.i64 h / 2) (f32.i64 w / 2) view_dist camera) triangles

  let close_enough_dist (p: point_projected): bool =
    0.0 <= p.z && p.z < draw_dist

  let close_enough_fully_out_of_frame
      ((p0, p1, p2): triangle_projected): bool =
    (p0.projected.x < 0 && p1.projected.x < 0 && p2.projected.x < 0) ||
    (p0.projected.x >= i32.i64 w && p1.projected.x >= i32.i64 w && p2.projected.x >= i32.i64 w) ||
    (p0.projected.y < 0 && p1.projected.y < 0 && p2.projected.y < 0) ||
    (p0.projected.y >= i32.i64 h && p1.projected.y >= i32.i64 h && p2.projected.y >= i32.i64 h)

  let close_enough (triangle: triangle_projected): bool =
    (close_enough_dist triangle.0 ||
     close_enough_dist triangle.1 ||
     close_enough_dist triangle.2) &&
    ! (close_enough_fully_out_of_frame triangle)

  let aux = map (.1) triangles_with_aux
  let (triangles_projected', aux') =
    unzip (filter (close_enough <-< (.0)) (zip triangles_projected aux))
  let triangles_slopes = prepare_triangles triangles_projected'
  in zip triangles_slopes aux'

-- | Render projected triangles using expand and reduce_by_index.
def render_projected_triangles [n] 'a
    (h: i64)
    (w: i64)
    (triangles_prepared: [n]triangle_slopes)
    (pixel_color: pixel_color_function a)
    (aux: [n]a)
    (aux_empty: a): [h][w]argb.colour =
  let aux' = zip (map i32.i64 (0..<n)) aux
  let points =
    lines_of_triangles triangles_prepared aux'
    |> points_of_lines
    |> filter (\(p, _) ->
                 let {x, y} = p.extra
                 in x >= 0 && x < i32.i64 w
                    && y >=0 && y < i32.i64 h)
  let coordinates =
    map (\(p, _) -> (i64.i32 p.extra.y, i64.i32 p.extra.x)) points
  let points =
    map (\(p, (aux_internal, aux)) ->
           let z_inv = interpolate p.bary triangles_prepared[aux_internal] (.extra.z_inv)
           let p' = {extra={i=aux_internal, z=1 / z_inv},
                     bary=p.bary}
           in (p', aux))
        points

  let z_check ((a, aux_a): (pixel_final, a))
              ((b, aux_b): (pixel_final, a)): (pixel_final, a) =
    if (a.extra.z >= 0 && a.extra.z < b.extra.z) || b.extra.z < 0
    then (a, aux_a)
    else (b, aux_b)

  let empty = ({extra={i= -1, z= -f32.inf},
                bary={u= -1, v= -1}}, aux_empty)
  in reduce_by_index_2d (replicate h (replicate w empty))
                        z_check empty coordinates points
     |> map (map pixel_color)
