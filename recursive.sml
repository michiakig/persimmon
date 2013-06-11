(* probably the simplest possible recursive descent parser

 S -> 0 S 1
 S -> 0 1

 *)

datatype token = One | Zero

datatype ast = Z | Succ of ast

val stream = [Zero, Zero, Zero, One, One, One]
val lookahead = ref stream

exception CharToTokenError

fun charToToken #"0" = Zero
  | charToToken #"1" = One
  | charToToken _ = raise CharToTokenError

fun show One = "1"
  | show Zero = "0"

exception SyntaxError of string

fun S []      = raise (SyntaxError "unexpected end of input")
  | S (_ :: []) = raise (SyntaxError "unexpected end of input")
  (* I think this grammar is not actually LL1,
   * here we need to lookahead two tokens
   *)
  | S (Zero :: One :: rest) = (Z, rest)
  | S (Zero :: cs) =
    let
       val (ast, rest) = S cs
    in
       if hd rest = One
          then (Succ ast, tl rest)
       else raise (SyntaxError "syntax error")
    end
  | S _ = raise (SyntaxError "syntax error")

fun parse s =
    let
       val (ast, _) = S (map charToToken (explode s))
    in
       ast
    end
