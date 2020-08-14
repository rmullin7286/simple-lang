import foo.bar.baz

type Point = { x : Int
               y : Int
             }

type PointF = { x : Float; y : Float }

type PointGeneric T = { x : T; y : T }

type Pair T U = {first : T; second : U}