structure Parser =
struct

   (* datatype optyp = Prefix | Infix | Postfix *)
   (* type tokinfo = {prec : int, typ : optyp } *)

   structure Map = BinaryMapFn(
      struct
         type ord_key = string
         val compare = String.compare
      end)

   val tokens = foldl (fn ((k, v), acc) => Map.insert (acc, k, v)) Map.empty [
          ("+", 50)
         ,("-", 50)
         ,("*", 60)
         ,("/", 60)
       ]

   exception NoPrecedence of string
   fun getPrec (Token.Op x) = valOf (Map.find (tokens, x))
     | getPrec t            = raise NoPrecedence (Token.show t)

   fun isInfix (Token.Op x) = true
     | isInfix _            = false

   exception SyntaxError of string

   fun parse (ts : Token.t list) : Syntax.t =
       let
          val rest = ref ts
          fun has () = not (null (!rest))
          fun peek () = case !rest of [] => NONE | t :: _ => SOME t
          fun unsafe () = hd (!rest)
          fun eat () = rest := tl (!rest)
          fun next () = peek () before eat ()
          fun match t =
              case peek () of
                  NONE => raise SyntaxError ("expected " ^ Token.show t)
                | SOME t' => if t = t' then eat () else raise SyntaxError ("expected " ^ Token.show t)

          val debug = true
          fun log s =
              if debug
                 then (print (s ^ "(" ^
                              (case peek () of
                                   NONE => ".."
                                 | SOME t => Token.show t)
                              ^ ")")
                      ; print "\n")
              else ()
          fun expected s t = raise SyntaxError ("expected " ^ s ^ 
                                                ", got " ^ Token.show t)
          (*
           * parse an atomic expression -- number or parenthesized infix
           * corresponds to `factors` in the recursive descent parser
           *)
          fun atom () : Syntax.t =
              (log "atom";
               case peek () of
                   SOME (Token.Num n) => (eat (); Syntax.Num n)
                 | SOME t             => expected "Num" t
                 | NONE               => raise SyntaxError "unexpected EOF")

          (*
           * parse an infix expression
           *)
          and infexp (prec : int) : Syntax.t =
              (log "infexp";
               let
                  val lhs = atom ()
               in
                  if has () andalso isInfix (unsafe ())
                     then infexp' (prec, lhs)
                  else lhs

               end)

          (* infix expression helper *)
          and infexp' (prec : int, lhs : Syntax.t) : Syntax.t =
              (log "infexp'";
               case peek () of
                   SOME t =>
                   let
                      val prec' = getPrec t
                   in
                      if prec < prec' andalso isInfix t
                         then let val t = next ()
                                  val SOME (Token.Op x) = t
                                  val lhs = Syntax.Infix (x, lhs, infexp prec')
                              in infexp' (prec, lhs)
                              end
                      else lhs
                   end
                 | _ => lhs)

       in
          infexp 0
       end
end
