structure Lexer =
struct

datatype token = LParen | RParen | Atom of string | Dot

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
             | SOME (#".", s') => SOME (Dot, s')
             | SOME (#"(", s') => SOME (LParen, s')
             | SOME (#")", s') => SOME (RParen, s')
             | SOME (_, s') =>
               case getAtom (rdr, stripws s) of
                   NONE => NONE
                 | SOME (atom, s') => SOME (Atom atom, s')
    in
       tokenize'
    end

end

structure Parser =
struct

datatype sexpr = SNil | SAtom of string | SCons of sexpr * sexpr | SList of sexpr list

datatype ('a, 'b) either = Success of 'a | Fail of 'b

(*
 * given a token reader, produce an sexpr (AST) reader
 *)
fun parse (rdr : (token, 'a) StringCvt.reader) : ((sexpr, string) either, 'a) StringCvt.reader =
    let
       exception SyntaxError of string * 'a

       fun unexpected s = raise (SyntaxError ("unexpected end of input", s))

       fun parseSexpr s =
           case rdr s of
               NONE => NONE
             | SOME (Atom a, s') => SOME (SAtom a, s')
             | SOME (LParen, s') => parseTail s'
             | SOME (RParen, s') => raise (SyntaxError ("unexpected )", s'))
             | SOME (Dot, s') => raise (SyntaxError ("unexpected .", s'))

       and parseTail s =
           case rdr s of
               NONE => unexpected s
             | SOME (RParen, s') => SOME (SNil, s')
             | _ => case parseSexpr s of
                        NONE => unexpected s
                      | SOME (hd, s') => parseCdr hd s'

       and parseCdr car s =
           case rdr s of
               NONE => unexpected s
             | SOME (Dot, s') => (case parseSexpr s' of
                                     NONE => unexpected s'
                                   | SOME (cdr, s'') => case rdr s'' of
                                                            SOME (RParen, s''') => SOME (SCons (car, cdr), s''')
                                                          | SOME _ => raise (SyntaxError ("expected )", s''))
                                                          | NONE => unexpected s'')
             | SOME (RParen, s') => SOME (SCons (car, SNil), s')
             | SOME _ => parseList [car] s

       and parseList acc s =
           case rdr s of
               NONE => unexpected s
             | SOME (RParen, s') => SOME (SList (rev acc), s')
             | _ => case parseSexpr s of
                        NONE => unexpected s
                      | SOME (sexpr, s') => parseList (sexpr :: acc) s'
    in
       fn s =>
          (case parseSexpr s of
               SOME (x, s') => SOME (Success x, s')
             | NONE => NONE)
          handle SyntaxError (msg, s') => SOME (Fail msg, s')
    end

end
