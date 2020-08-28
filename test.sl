import foo.bar.baz

type Point = { x : Int
               y : Int
             }

type Pair t u = { x : t;
                  y : u }

let foo (p : Point) : String = "foo"