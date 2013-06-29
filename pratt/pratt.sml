structure Pratt =
struct

   structure Token =
   struct
      datatype t = Num of int | Op of string | LParen | RParen
      fun show (Num n) = "Num " ^ Int.toString n
        | show (Op x) = "Op " ^ x
        | show LParen = "LParen"
        | show RParen = "RParen"
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
                      | symbol x = Char.isPunct x
                    val (opStr, rest) = takeWhile symbol chars
                 in
                    (String.implode opStr, rest)
                 end
      
             fun getDigit chars =
                 let
                    val (numStr, rest) = takeWhile Char.isDigit chars
                 in
                    (Int.fromString (String.implode numStr), rest)
                 end
      
             fun lexStr' acc [] = List.rev acc
               | lexStr' acc (#"("::xs) = lexStr' (LParen :: acc) xs
               | lexStr' acc (#")"::xs) = lexStr' (RParen :: acc) xs
               | lexStr' acc (all as x::xs) =
                 if Char.isSpace x
                    then lexStr' acc xs
                 else if Char.isDigit x
                    then case getDigit all of
                             (SOME n, rest) => lexStr' (Num n :: acc) rest
                           | (NONE, _) => raise LexicalError
                 else if Char.isPunct x
                    then case getOp all of
                             (oper, rest) => lexStr' (Op oper :: acc) rest
                 else raise LexicalError
          in
             lexStr' [] (String.explode s)
          end
      end
   end

   structure Syntax =
   struct
      datatype t = Num of int
                 | Infix of string * t * t
                 | Unary of string * t
   end

   (*
    * Pratt parser for arithmetic expressions
    *)
   structure Parser =
   struct
   
      datatype optyp = Prefix | Infix | Postfix
      type tokinfo = {prec : int, typ : optyp }
   
      structure Map = BinaryMapFn(
         struct
            type ord_key = string
            val compare = String.compare
         end)
   
      val tokens = foldl (fn ((k, v), acc) => Map.insert (acc, k, v)) Map.empty [
             ("+", {prec=50,typ=Infix})
            ,("-", {prec=50,typ=Infix})
            ,("*", {prec=60,typ=Infix})
            ,("/", {prec=60,typ=Infix})
            ,("^", {prec=70,typ=Infix})
            ,("~", {prec=70,typ=Prefix})
            ,("++", {prec=70,typ=Postfix})
            ,("--", {prec=70,typ=Postfix})
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
              * parse an atomic expression -- number or parenthesized infix
              * corresponds to `factors` in the recursive descent parser
              *)
             fun atom () : Syntax.t =
                 (log "atom";
                  case peek () of
                      SOME (Token.Num n) => (eat (); Syntax.Num n)
                    | SOME Token.LParen  => (eat (); infexp 0 before match Token.RParen)
                    | SOME (t as Token.Op x) => (eat (); case getPrec t of
                                                             {prec=_, typ=Prefix} => Syntax.Unary (x, infexp 70)
                                                           | _ => raise SyntaxError "expected prefix op")
                    | SOME t             => expected "Num or LParen" t
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
                                   {prec=prec', typ=Infix} =>
                                   if prec < prec'
                                      then let val _ = eat ();
                                               val lhs = Syntax.Infix (x, lhs, infexp prec')
                                           in infexp' (prec, lhs)
                                           end
                                   else lhs
                                 | {prec=prec', typ=Postfix} => (eat (); infexp' (prec, Syntax.Unary (x, lhs)))
                                 | _ => raise SyntaxError "expected infix or postfix op")
                            | _ => lhs)
                  in
                     infexp' (prec, atom ())
                  end)
          in
             infexp 0
          end
   end

end
