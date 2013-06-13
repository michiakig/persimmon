(* grammar:
E  -> T E'
E' -> + E
E' ->
T  -> F T'
T' -> * T
T' ->
F  -> ( E )
F  -> id
*)

structure Lexer =
struct

datatype t = Num of int
           | LParen
           | RParen
           | Add
           | Mul
           (* | Div *)
           (* | Sub *)

fun show (Num n) = "Num " ^ Int.toString n
  | show LParen = "LParen"
  | show RParen = "RParen"
  | show Add = "Add"
  | show Mul = "Mul"
  (* | show Div = "Div" *)
  (* | show Sub = "Sub" *)

local

fun takeWhile p xs =
    let
       fun takeWhile' acc [] = (rev acc, [])
         | takeWhile' acc (all as x::xs) =
           if p x
              then takeWhile' (x::acc) xs
           else (rev acc, all)
    in
       takeWhile' [] xs
    end

fun getDigit chars =
    let
       val (numStr, rest) = takeWhile Char.isDigit chars
    in
       (Int.fromString (String.implode numStr), rest)
    end

exception LexicalError of string

in

fun lex (#"(" :: rest) = LParen :: lex rest
  | lex (#")" :: rest) = RParen :: lex rest
  | lex (#"+" :: rest) = Add :: lex rest
  (* | lex (#"-" :: rest) = Sub :: lex rest *)
  | lex (#"*" :: rest) = Mul :: lex rest
  (* | lex (#"/" :: rest) = Div :: lex rest *)
  | lex (all as c :: cs) =
    if Char.isDigit c
       then case getDigit all of
                (SOME n, rest) => (Num n) :: lex rest
              | (NONE, _) =>
                raise LexicalError ("error lexing num: " ^ String.implode all)
    else if Char.isSpace c
            then lex cs
         else raise LexicalError ("unknown char: " ^ Char.toString c)
  | lex [] = []

end
end

structure Parser =
struct

structure L = Lexer

datatype ast = Num of int
             | Add of ast * ast
             | Mul of ast * ast
             (* | Div of ast * ast *)
             (* | Sub of ast * ast *)

fun isNum (L.Num _) = true
  | isNum _ = false

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
       fun err s = raise SyntaxError ("err " ^ s)

       fun expr () = (term (); expr' ())
       and term () = (factor (); term'())
       and expr' () = if match L.Add then (next (); expr()) else ()
       and term' () = if match L.Mul then (next (); term()) else ()
       and factor () = if match L.LParen
                          then (next ()
                               ; expr ()
                               ; if match L.RParen
                                    then adv ()
                                 else err ")")
                       else if has () andalso isNum (peek ())
                               then adv ()
                            else err "digit"
    in
       expr () handle SyntaxError s => print ("SyntaxError: " ^ s ^ "\n")
    end

fun top s = parse (L.lex (String.explode s))

end
