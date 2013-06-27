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
