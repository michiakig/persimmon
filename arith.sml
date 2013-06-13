(* grammar:
E  -> T E'
E' -> + T E'
E' ->
T  -> F T'
T' -> * F T'
T' ->
F  -> ( E )
F  -> id
*)

datatype token =
         Num
       | LParen
       | RParen
       | Add
       | Mul
       | Div
       | Sub

fun show Num = "Num"
  | show LParen = "LParen"
  | show RParen = "RParen"
  | show Add = "Add"
  | show Mul = "Mul"
  | show Div = "Div"
  | show Sub = "Sub"

exception SyntaxError of string
fun parse toks =
    let
       val idx = ref 0
       val arr = Array.fromList toks
       fun has () = !idx < Array.length arr
       fun adv () = idx := !idx + 1
       fun next () = Array.sub (arr, !idx) before adv ()
       fun peek () = Array.sub (arr, !idx)
       fun match tok = has () andalso tok = peek ()
       fun expected (tok : token) : unit = raise SyntaxError ("expected " ^ show tok)

       fun expr () = (term (); expr' ())
       and term () = (factor (); term'())
       and expr' () = if match Add then (next (); term (); expr'()) else ()
       and term' () = if match Mul then (next (); factor (); term'()) else ()
       and factor () = if match LParen
                          then (next ()
                               ; expr ()
                               ; if match RParen
                                    then adv ()
                                 else expected RParen)
                       else if match Num
                               then adv ()
                            else expected Num
    in
       expr () handle SyntaxError s => print ("SyntaxError: " ^ s ^ "\n")
    end
