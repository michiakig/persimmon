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
