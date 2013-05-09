(* probably the simplest possible recursive descent parser
 * S -> 0 S 1
 * S -> 0 1
 *)

datatype token = One | Zero

val stream = [Zero, Zero, Zero, One, One, One]
val lookahead = ref stream

fun show One = "1"
  | show Zero = "0"

exception SyntaxError of string

fun match token =
    case !lookahead of
        [] => raise (SyntaxError "unexpected end of input")
      | (t :: ts) =>
        if t = token
           then (print (show token)
                ; lookahead := ts)
        else raise (SyntaxError ("expected: " ^
                                 show token ^
                                 ", but got: " ^
                                 show t))

fun S () =
    case !lookahead of
        [] => raise (SyntaxError "unexpected end of input")
      | (_ :: []) => raise (SyntaxError "unexpected end of input")
      (* I think this grammar is not actually LL1,
       * here we need to lookahead two tokens
       *)
      | (Zero :: One :: _) => (match Zero; match One)
      | _ => (match Zero; S (); match One)

fun parse () = (S (); print "\n")
