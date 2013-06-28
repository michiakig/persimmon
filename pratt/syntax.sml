structure Syntax =
struct
   datatype t = Num of int
              | Add of t * t
              | Sub of t * t
              | Mul of t * t
              | Div of t * t
              | Pow of t * t
              | Neg of t
              | Incr of t
              | Decr of t
end
