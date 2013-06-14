(* grammar:
expr   -> term expr'
expr'  -> + expr
expr'  -> - expr
expr'  ->
term   -> factor term'
term'  -> * term
term'  -> / term
term'  ->
factor -> ( expr )
factor -> id
*)

structure Lexer =
struct

datatype t = Num of int
           | LParen
           | RParen
           | Add
           | Mul
           | Div
           | Sub

fun show (Num n) = "Num " ^ Int.toString n
  | show LParen = "LParen"
  | show RParen = "RParen"
  | show Add = "Add"
  | show Mul = "Mul"
  | show Div = "Div"
  | show Sub = "Sub"

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

fun lex (s : string) : t list =
    let
       fun lex' acc (#"(" :: rest) = lex' (LParen :: acc) rest
         | lex' acc (#")" :: rest) = lex' (RParen :: acc) rest
         | lex' acc (#"+" :: rest) = lex' (Add :: acc) rest
         | lex' acc (#"-" :: rest) = lex' (Sub :: acc) rest
         | lex' acc (#"*" :: rest) = lex' (Mul :: acc) rest
         | lex' acc (#"/" :: rest) = lex' (Div :: acc) rest
         | lex' acc (all as c :: cs) =
           if Char.isDigit c
              then case getDigit all of
                       (SOME n, rest) => lex' ((Num n) :: acc) rest
                     | (NONE, _) =>
                       raise LexicalError ("error lexing num: " ^ String.implode all)
           else if Char.isSpace c
                   then lex' acc cs
                else raise LexicalError ("unknown char: " ^ Char.toString c)
         | lex' acc [] = rev acc
    in
       lex' [] (String.explode s)
    end
end
end

structure Parser =
struct

structure L = Lexer

datatype ast = Num of int
             | Add of ast * ast
             | Mul of ast * ast
             | Div of ast * ast
             | Sub of ast * ast

fun show (Num n) = "Num n"
  | show (Add (lhs, rhs)) = "Add (" ^ show lhs ^ "," ^ show rhs ^ ")"
  | show (Sub (lhs, rhs)) = "Sub (" ^ show lhs ^ "," ^ show rhs ^ ")"
  | show (Mul (lhs, rhs)) = "Mul (" ^ show lhs ^ "," ^ show rhs ^ ")"
  | show (Div (lhs, rhs)) = "Div (" ^ show lhs ^ "," ^ show rhs ^ ")"

fun isNum (L.Num _) = true
  | isNum _ = false

exception SyntaxError of string
fun parse toks =
    let
       fun error s = raise SyntaxError s
       val debug = false
       fun log (s, []) = if debug then print (s ^ "(..)\n") else ()
         | log (s, t :: ts) = if debug then print (s ^ "(" ^ L.show t ^ ")\n") else ()

       fun expr (toks : L.t list) : (ast * L.t list) =
           (log ("expr", toks);
            let
               val (lhs, rest) = term toks
            in
               case expr' rest of
                   (NONE, rest') => (lhs, rest')
                 | (SOME (oper, rhs), rest') => (oper (lhs, rhs), rest')
            end)

       and term (toks : L.t list) : (ast * L.t list) =
           (log ("term", toks);
            let
               val (lhs, rest) = factor toks
            in
               case term' rest of
                   (NONE, rest') => (lhs, rest')
                 | (SOME (oper, rhs), rest') => (oper (lhs, rhs), rest')
            end)

       and expr' toks : (((ast * ast -> ast) * ast) option * L.t list) =
           (log ("expr'", toks);
            let
               fun cont oper (rhs, rest') = (SOME (oper, rhs), rest')
            in
               case toks of
                   L.Add :: rest => cont Add (expr rest)
                 | L.Sub :: rest => cont Sub (expr rest)
                 (* | tok :: rest => error ("(expr') unexpected " ^ L.show tok) *)
                 | _ => (NONE, toks)
            end)

       and term' toks : (((ast * ast -> ast) * ast) option * L.t list) =
           (log ("term'", toks);
           let
              fun cont oper (rhs, rest') = (SOME (oper, rhs), rest')
           in
              case toks of
                  L.Mul :: rest => cont Mul (term rest)
                | L.Div :: rest => cont Div (term rest)
                (* | tok :: rest => error ("(term') unexpected " ^ L.show tok) *)
                | _ => (NONE, toks)
           end)

       and factor (toks : L.t list) : ast * L.t list =
           (log ("factor", toks);
            case toks of
                L.LParen :: rest => (case expr rest of
                                         (ast, L.RParen :: rest') => (ast, rest')
                                       | _ => error "(factor) expected ')'")
              | (L.Num n) :: rest => (Num n, rest)
              | _ => error "(factor) expected digit")
    in
       case expr toks of
           (ast, []) => ast
         | (_, rest) => raise (SyntaxError ("incomplete parse: " ^
                                            (String.concatWith "," (map L.show rest))))
    end

end
