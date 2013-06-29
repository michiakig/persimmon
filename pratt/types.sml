(* 
 * Pratt parser for type expressions
 *)
structure PrattTypes =
struct

   structure Token =
   struct
      datatype t = Var of string | Con of string | Op of string | LParen | RParen
      fun show (Var v) = "Var '" ^ v
        | show (Con c) = "Con " ^ c
        | show (Op x)  = "Op " ^ x
        | show LParen  = "LParen"
        | show RParen  = "RParen"
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
   
             fun getOp chars =
                 let
                    fun symbol #"(" = false
                      | symbol #")" = false
                      | symbol #"'" = false
                      | symbol x = Char.isPunct x
                    val (opStr, rest) = takeWhile symbol chars
                 in
                    (String.implode opStr, rest)
                 end
      
             fun getVar chars =
                 let
                    val (var, rest) = takeWhile Char.isAlpha chars
                 in
                    (String.implode var, rest)
                 end
      
             fun lexStr' acc [] = List.rev acc
               | lexStr' acc (#"("::xs) = lexStr' (LParen :: acc) xs
               | lexStr' acc (#")"::xs) = lexStr' (RParen :: acc) xs
               | lexStr' acc (#"'"::xs) = (case getVar xs of
                                               (v, rest) => lexStr' (Var v :: acc) rest)
               | lexStr' acc (all as x::xs) =
                 if Char.isSpace x
                    then lexStr' acc xs
                 else if Char.isPunct x
                    then case getOp all of
                             (oper, rest) => lexStr' (Op oper :: acc) rest
                 else case getVar all of
                          (var, rest) => lexStr' (Con var :: acc) rest
          in
             lexStr' [] (String.explode s)
          end
      end
   end

   structure Syntax =
   struct
      datatype t = Var of string
                 | Con of string * t
                 | Arrow of t * t
                 | Tuple of t list
                 | Paren of t
   end

   (*
    * Pratt parser for type expressions
    *)
   structure Parser =
   struct

      (*
       * tuple `*` has higher precedence than arrow `->`
       *)
   
      type binctor = Syntax.t * Syntax.t -> Syntax.t
      type unctor = Syntax.t -> Syntax.t
      datatype assoc = Left | Right
      datatype optyp = Prefix of int * unctor | Infix of int * binctor * assoc | Postfix of int * unctor
   
      structure Map = BinaryMapFn(
         struct
            type ord_key = string
            val compare = String.compare
         end)
   
      val tokens = foldl (fn ((k, v), acc) => Map.insert (acc, k, v)) Map.empty [
             ("*", Infix (60, fn (Syntax.Tuple xs, y) => Syntax.Tuple (xs @ [y]) | (x, y) => Syntax.Tuple [x, y], Left))
            ,("->", Infix (50, Syntax.Arrow, Right))
          ]
   
      exception NoPrecedence of string
      fun getPrec (Token.Op x) = (case Map.find (tokens, x) of SOME info => info | _ => raise NoPrecedence x)
        | getPrec t            = raise NoPrecedence (Token.show t)
   
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
              * parse an atomic expression -- var or parenthesized infix
              *)
             fun atom () : Syntax.t =
                 (log "atom";
                  case peek () of
                      SOME (Token.Var v) => (eat (); Syntax.Var v)
                    | SOME Token.LParen  => (eat (); Syntax.Paren (infexp 0) before match Token.RParen)
                    | SOME t             => expected "Var or LParen" t
                    | NONE               => raise SyntaxError "unexpected EOF")
   
             (*
              * parse an infix expression
              *)
             and infexp (prec : int) : Syntax.t =
                 (log "infexp";
                  let
                     fun infexp' (prec : int, lhs : Syntax.t) : Syntax.t =
                         (log "infexp'";
                          case peek () of
                              SOME (t as Token.Op x) =>
                              (case getPrec t of
                                   Infix (prec', ctor, assoc) =>
                                   if prec < prec'
                                      then let val _ = eat ();
                                               val prec'' = case assoc of Left => prec' | Right => prec' - 1
                                               val lhs = ctor (lhs, infexp prec'')
                                           in infexp' (prec, lhs)
                                           end
                                   else lhs
                                 | _ => raise SyntaxError "expected infix or postfix op")
                            | SOME (Token.Con c) => (eat (); infexp' (prec, Syntax.Con (c, lhs)))
                            | _ => lhs)
                  in
                     infexp' (prec, atom ())
                  end)
          in
             infexp 0
          end
   end

   fun parse s = Parser.parse (Lexer.lexStr s)

end
