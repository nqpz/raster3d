import "../types"
import "../transformations"

def vec3_same f = {x=f, y=f, z=f}
def vec3_one = vec3_same vec3.one

module transf = {
  def rotate' angle origo = map (rotate_triangle angle origo)
  def rotate angle = rotate' angle vec3.zero
  def translate offset = map (translate_triangle offset)
  def scale s = map (scale_triangle s)
}

module shape = {
  open transf

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
