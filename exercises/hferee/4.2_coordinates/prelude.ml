type cartesian = { x: float; y: float; }
type polar = { r: float; angle: float; }


type point = Cartesian of cartesian | Polar of polar
