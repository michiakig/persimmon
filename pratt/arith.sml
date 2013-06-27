structure Arith =
struct

   structure Token =
   struct
      datatype t = Num of int | Add | Sub | Mul | Div | LParen | RParen | Tilde | Caret | Incr | Decr
      fun show (Num n) = "Num " ^ Int.toString n
        | show Add = "Add"
        | show Sub = "Sub"
        | show Mul = "Mul"
        | show Div = "Div"
        | show LParen = "LParen"
        | show RParen = "RParen"
        | show Tilde = "Tilde"
        | show Caret = "Caret"
        | show Incr = "Incr"
        | show Decr = "Decr"
   end

   structure Lexer =
   struct
      exception LexicalError
      local
         open Token
      in
      fun lexStr s =
          let
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

             fun lexStr' acc [] = List.rev acc
               | lexStr' acc (#"+":: #"+"::xs) = lexStr' (Incr::acc) xs
               | lexStr' acc (#"+"::xs)       = lexStr' (Add::acc) xs
               | lexStr' acc (#"-":: #"-"::xs) = lexStr' (Decr::acc) xs
               | lexStr' acc (#"-"::xs)       = lexStr' (Sub::acc) xs
               | lexStr' acc (#"*"::xs)       = lexStr' (Mul::acc) xs
               | lexStr' acc (#"/"::xs)       = lexStr' (Div::acc) xs
               | lexStr' acc (#"("::xs)       = lexStr' (LParen::acc) xs
               | lexStr' acc (#")"::xs)       = lexStr' (RParen::acc) xs
               | lexStr' acc (#"~"::xs)       = lexStr' (Tilde::acc) xs
               | lexStr' acc (#"^"::xs)       = lexStr' (Caret::acc) xs
               | lexStr' acc (all as x::xs) =
                 if Char.isSpace x
                    then lexStr' acc xs
                 else if Char.isDigit x
                    then case getDigit all of
                             (SOME n, rest) => lexStr' (Num n :: acc) rest
                           | (NONE, _) => raise LexicalError
                 else raise LexicalError
          in
             lexStr' [] (String.explode s)
          end
      end
   end

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

   structure Parser =
   struct
      exception SyntaxError

      fun parse (tokens : Token.t list) : Syntax.t =
          let
             val rest = ref tokens
             fun has () = null (!rest)
             fun peek () = case !rest of [] => NONE | t :: _ => SOME t
             fun eat () = rest := tl (!rest)
             fun next () = peek () before eat ()
             fun match t =
                 case peek () of
                     NONE => raise SyntaxError
                   | SOME t' => if t = t' then eat () else raise SyntaxError

             val debug = false
             fun log s =
                 if debug
                    then (print (s ^ "(" ^
                                 (case peek () of
                                      NONE => ".."
                                    | SOME t => Token.show t)
                                 ^ ")")
                         ; print "\n")
                 else ()

             fun expr () : Syntax.t =
                 (log "expr"; expr' (term ()))

             and expr' (lhs : Syntax.t) : Syntax.t =
                 (log "expr'";
                  case peek () of
                      SOME Token.Add => (eat (); expr' (Syntax.Add (lhs, term ())))
                    | SOME Token.Sub => (eat (); expr' (Syntax.Sub (lhs, term ())))
                    | _ => lhs)

             and term () : Syntax.t =
                 (log "term"; term' (pow ()))

             and term' (lhs : Syntax.t) : Syntax.t =
                 (log "term'";
                  case peek () of
                      SOME Token.Mul => (eat (); term' (Syntax.Mul (lhs, pow ())))
                    | SOME Token.Div => (eat (); term' (Syntax.Div (lhs, pow ())))
                    | _ => lhs)

             and pow () : Syntax.t =
                 (log "pow"; pow' (factor ()))

             and pow' (lhs : Syntax.t) : Syntax.t =
                 (log "pow'";
                  case peek () of
                      SOME Token.Caret => (eat (); pow' (Syntax.Pow (lhs, factor ())))
                    | _ => lhs)

             and factor () : Syntax.t =
                 (log "factor";
                  let
                     val fac =
                  case next () of
                      SOME Token.LParen =>
                      let
                         val ast = expr ()
                         val _ = match Token.RParen
                      in
                         ast
                      end
                    | SOME (Token.Num n) => Syntax.Num n
                    | SOME Token.Tilde => Syntax.Neg (expr ())
                    | SOME t => raise SyntaxError
                    | _ => raise SyntaxError
                  in
                     case peek () of
                         SOME Token.Incr => (eat (); Syntax.Incr fac)
                       | SOME Token.Decr => (eat (); Syntax.Decr fac)
                       | _ => fac
                  end)
          in
             expr ()
          end
   end

   fun parse s = Parser.parse (Lexer.lexStr s)

end
