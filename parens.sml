(*
  S -> e
  S -> R ( S ) S
  R -> e
  R -> ( S )
*)

datatype token = LParen | RParen

datatype ast = E | C of ast * ast * ast

exception LexError
fun lex s =
    let
       fun lex' acc [] = List.rev acc
         | lex' acc (#"(" :: cs) = lex' (LParen :: acc) cs
         | lex' acc (#")" :: cs) = lex' (RParen :: acc) cs
         | lex' acc (c :: cs) =
           if Char.isSpace c
              then raise LexError
           else lex' acc cs
    in
       lex' [] (String.explode s)
    end

exception SyntaxError

fun match x [] = false
  | match x (y :: ys) = x = y

fun parse toks =
    let
       val idx = ref 0
       val arr = Array.fromList toks
       fun peek () = if !idx < Array.length arr
                        then SOME (Array.sub (arr, !idx))
                     else NONE
       fun advance () = idx := !idx + 1
       fun match t =
           case peek () of
               SOME t' => if t = t' then (advance (); true) else false
             | NONE => false
       fun S () = if match LParen
                     then R ()
                  else true
       and R () = if S ()
                     then match RParen andalso S ()
                  else raise SyntaxError
    in
       S ()
    end
