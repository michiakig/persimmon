datatype token = LParen | RParen | Atom of string

(*
 * Given a char reader and stream, try to extract a Scheme atom (string) from the stream, and return it with the rest of the stream
 *)
fun getAtom (rdr : (char, 'b) StringCvt.reader, s : 'b) : (string * 'b) option =
    let
       fun return [] _ = NONE
         | return acc s = SOME (String.implode (rev acc), s)

       fun getAtom' acc s =
           case rdr s of
                NONE => return acc s
              | SOME (#"(", rest) => return acc s
              | SOME (#")", rest) => return acc s
              | SOME (x, rest) => if Char.isSpace x then
                                     return acc s
                                  else getAtom' (x :: acc) rest
    in
       getAtom' [] s
    end

(*
 * Given a char reader, produce a token reader)
 *)
fun tokenize (rdr : (char, 'a) StringCvt.reader) : (token, 'a) StringCvt.reader =
    let
       fun stripws s = StringCvt.dropl Char.isSpace rdr s

       fun tokenize' s =
           case rdr (stripws s) of
               NONE => NONE
             | SOME (#"(", s') => SOME (LParen, s')
             | SOME (#")", s') => SOME (RParen, s')
             | SOME (_, s') =>
               case getAtom (rdr, stripws s) of
                   NONE => NONE
                 | SOME (atom, s') => SOME (Atom atom, s')
    in
       tokenize'
    end

datatype sexpr = SAtom of string | SList of sexpr list

datatype ('a, 'b) either = Success of 'a | Fail of 'b

(*
 * given a token reader, produce an sexpr (AST) reader
 *)
fun parse (rdr : (token, 'a) StringCvt.reader) : ((sexpr, string) either, 'a) StringCvt.reader =
    let
       exception SyntaxError of string * 'a

       fun parseList acc s =
           case rdr s of
               SOME (Atom a, s') => parseList (SAtom a :: acc) s'
             | SOME (LParen, _) => (case parseSexp s of
                                        NONE => raise (SyntaxError ("expected Atom or RParen", s))
                                      | SOME (sexp, s') => parseList (sexp :: acc) s')
             | _ => (rev acc, s)

       and parseSexp s =
           case rdr s of
               NONE => NONE
             | SOME (LParen, s') =>
               (case parseList [] s' of
                    (inside, s'') =>
                    case rdr s'' of
                        SOME (RParen, s''') => SOME (SList inside, s''')
                      | _ => raise (SyntaxError ("expected RParen", s'')))
             | SOME (Atom a, s') => SOME (SAtom a, s')
             | SOME (RParen, _) => raise (SyntaxError ("expected LParen or Atom, got RParen", s))
    in
       fn s =>
          (case parseSexp s of
               SOME (x, s') => SOME (Success x, s')
             | NONE => NONE)
          handle SyntaxError (msg, s') => SOME (Fail msg, s')
    end
