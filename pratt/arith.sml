structure Arith =
struct
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
