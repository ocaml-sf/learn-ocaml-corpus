open Vg
open Gg

type path = Vg.path
type image = Vg.image

let area = `O { P.o with P.width = 0.004}

let go_to x y = P.line ~rel:true (V2.v x y)

let triangle size (p : path) : path =
  p |> P.line ~rel:true (P2.v size 0.)
  |> P.line ~rel:true V2.(polar size (2. *. Float.pi /. 3.))
  |> P.line ~rel:true V2.(polar size (- 2. *. Float.pi /. 3.))

let move_by x y : path -> path = P.sub ~rel:true (V2.v x y)

let draw (p : path -> path) : image = I.cut ~area (p P.empty) (I.const Color.black)
