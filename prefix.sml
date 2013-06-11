(*
S -> + S S
T -> - S S
R -> a
*)

datatype token = Plus | Minus | A

datatype ast = AstPlus of ast * ast | AstMinus of ast * ast | AstA

exception LexError
fun lex s =
    let
       fun lex' acc [] = List.rev acc
         | lex' acc (#"+" :: cs) = lex' (Plus :: acc) cs
         | lex' acc (#"-" :: cs) = lex' (Minus :: acc) cs
         | lex' acc (#"a" :: cs) = lex' (A :: acc) cs
         | lex' acc (c :: cs) =
           if Char.isSpace c
              then lex' acc cs
           else raise LexError
    in
       lex' [] (String.explode s)
    end

exception SyntaxError
fun parse toks = 
    let
       fun parse' (A :: toks) = (AstA, toks)
         | parse' (Plus :: toks) =
           let
              val (l, rest) = parse' toks
              val (r, rest') = parse' rest
           in
              (AstPlus (l, r), rest')
           end
         | parse' (Minus :: toks) =
           let
              val (l, rest) = parse' toks
              val (r, rest') = parse' rest
           in
              (AstMinus (l, r), rest')
           end
         | parse' [] = raise SyntaxError

       val (ast, _) = parse' toks
    in
       ast
    end
