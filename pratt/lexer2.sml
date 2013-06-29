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

          local
             val ops = String.explode "+-*/"
          in
              fun isInfix char = List.exists (fn x => x = char) ops
          end
   
          fun getDigit chars =
              let
                 val (numStr, rest) = takeWhile Char.isDigit chars
              in
                 (Int.fromString (String.implode numStr), rest)
              end
   
          fun lexStr' acc [] = List.rev acc
            | lexStr' acc (all as x::xs) =
              if Char.isSpace x
                 then lexStr' acc xs
              else if Char.isDigit x
                 then case getDigit all of
                          (SOME n, rest) => lexStr' (Num n :: acc) rest
                        | (NONE, _) => raise LexicalError
              else if isInfix x
                 then lexStr' (Op (Char.toString x) :: acc) xs
              else raise LexicalError
       in
          lexStr' [] (String.explode s)
       end
   end
end
