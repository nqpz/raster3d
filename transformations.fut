import "types"

def translate_triangle
    (offset: vec3.vector)
    ((p, q, r): triangle)
    : triangle =
  vec3.((p + offset, q + offset, r + offset))

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

def rotate_triangle
    (angle: vec3.vector) (origo: vec3.vector)
    ((p, q, r): triangle)
    : triangle =
  (rotate_point angle origo p,
   rotate_point angle origo q,
   rotate_point angle origo r)

def scale_point (scale: vec3.vector) (p: vec3.vector): vec3.vector =
  scale vec3.* p

def scale_triangle (scale: vec3.vector) ((p, q, r): triangle): triangle =
  (scale_point scale p,
   scale_point scale q,
   scale_point scale r)
