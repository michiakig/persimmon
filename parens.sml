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

exception SyntaxError

(*
 * given a token reader, produce an sexpr (AST) reader
 *)
fun parse (rdr : (token, 'a) StringCvt.reader) : (sexpr, 'a) StringCvt.reader =
    let
       fun parseList acc s =
           case rdr s of
               SOME (Atom a, s') => parseList (SAtom a :: acc) s'
             | SOME (LParen, _) => (case parseSexp s of
                                        NONE => raise SyntaxError
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
                      | _ => raise SyntaxError)
             | SOME (Atom a, s') => SOME (SAtom a, s')
             | SOME (RParen, _) => raise SyntaxError
    in
       parseSexp
    end
