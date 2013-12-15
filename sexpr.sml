structure Lexer =
struct

datatype token = LParen | RParen | Atom of string | Dot

(*
 * Given a char reader and stream, try to extract a Scheme atom
 * (string) from the stream, and return it with the rest of the stream
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
 * Given a char reader, produce a token reader
 *)
fun tokenize (rdr : (char, 'a) StringCvt.reader) : (token, 'a) StringCvt.reader =
    fn s =>
       case rdr (StringCvt.skipWS rdr s) of
           NONE => NONE
         | SOME (#".", s') => SOME (Dot, s')
         | SOME (#"(", s') => SOME (LParen, s')
         | SOME (#")", s') => SOME (RParen, s')
         | SOME (_, s') =>
           case getAtom (rdr, StringCvt.skipWS rdr s) of
               NONE => NONE
             | SOME (atom, s') => SOME (Atom atom, s')

end

structure Parser =
struct

datatype sexpr = SNil | SAtom of string | SCons of sexpr * sexpr | SList of sexpr list

datatype ('a, 'b) either = Success of 'a | Fail of 'b



(* sexpr -> atom *)
(* sexpr -> ( inside ) *)

(* inside -> *)
(* inside -> sexpr tail *)

(* tail -> . sexpr *)
(* tail -> inside *)

(*
 * given a token reader, produce an sexpr (AST) reader
 *)
fun parse (rdr : (Lexer.token, 'a) StringCvt.reader) : ((sexpr, string) either, 'a) StringCvt.reader =
    let
       exception SyntaxError of string * 'a

       fun unexpected s = raise (SyntaxError ("unexpected end of input", s))

       fun parseSexpr s =
           case rdr s of
               NONE => NONE
             | SOME (Lexer.Atom a, s') => SOME (SAtom a, s')
             | SOME (Lexer.LParen, s') => parseTail s'
             | SOME (Lexer.RParen, s') => raise (SyntaxError ("unexpected )", s'))
             | SOME (Lexer.Dot, s') => raise (SyntaxError ("unexpected .", s'))

       and parseTail s =
           case rdr s of
               NONE => unexpected s
             | SOME (Lexer.RParen, s') => SOME (SNil, s')
             | _ => case parseSexpr s of
                        NONE => unexpected s
                      | SOME (hd, s') => parseCdr hd s'

       and parseCdr car s =
           case rdr s of
               NONE => unexpected s
             | SOME (Lexer.Dot, s') => (case parseSexpr s' of
                                     NONE => unexpected s'
                                   | SOME (cdr, s'') => case rdr s'' of
                                                            SOME (Lexer.RParen, s''') => SOME (SCons (car, cdr), s''')
                                                          | SOME _ => raise (SyntaxError ("expected )", s''))
                                                          | NONE => unexpected s'')
             | SOME (Lexer.RParen, s') => SOME (SCons (car, SNil), s')
             | SOME _ => parseList [car] s

       and parseList acc s =
           case rdr s of
               NONE => unexpected s
             | SOME (Lexer.RParen, s') => SOME (SList (rev acc), s')
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

val _ = use "reader.sml" ;

local
   open Reader
   open Lexer
   open Parser
in
   val [Atom "foo"]                 = consume (tokenize string) "foo"
   val [LParen, RParen]             = consume (tokenize string) "()"
   val [LParen, Atom "foo", RParen] = consume (tokenize string) "(foo)"

   val [Success (SAtom "foo")]                      = consume (parse (tokenize string)) "foo"
   val [Success SNil]                               = consume (parse (tokenize string)) "()"
   val [Success (SCons (SAtom "foo", SNil))]        = consume (parse (tokenize string)) "(foo)"
   val [Success (SCons (SAtom "foo", SAtom "bar"))] = consume (parse (tokenize string)) "(foo . bar)"
   val [Success (SList [SAtom "foo", SAtom "bar"])] = consume (parse (tokenize string)) "(foo bar)"
end
