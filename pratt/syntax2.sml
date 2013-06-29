structure Syntax =
struct
   datatype t = Num of int
              | Infix of string * t * t
              | Unary of string * t
end
