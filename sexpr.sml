(* Lexer and parser for simplified s-expressions (no dotted-pairs) *)

type ('a, 'b) reader = 'b -> ('a * 'b) option

structure Lexer =
struct

datatype token = LParen | RParen | Atom of string

(*
 * Given a char reader and stream, try to extract a Scheme atom
 * (string) from the stream, and return it with the rest of the stream
 *)
fun getAtom (rdr : (char, 'b) reader, s : 'b) : (string * 'b) option =
    let
       fun done [] _ = NONE
         | done acc s = SOME (String.implode (rev acc), s)

       fun getAtom' acc s =
           case rdr s of
                NONE => done acc s
              | SOME (#"(", rest) => done acc s
              | SOME (#")", rest) => done acc s
              | SOME (x,    rest) =>
                if Char.isSpace x then
                   done acc s
                else getAtom' (x :: acc) rest
    in
       getAtom' [] s
    end

(*
 * Given a char reader, produce a token reader
 *)
fun tokenize (rdr : (char, 'a) reader) : (token, 'a) reader =
    fn s =>
       case rdr (StringCvt.skipWS rdr s) of
           NONE => NONE
         | SOME (#"(", s') => SOME (LParen, s')
         | SOME (#")", s') => SOME (RParen, s')
         | SOME (_, s') =>
           case getAtom (rdr, StringCvt.skipWS rdr s) of
               NONE => NONE
             | SOME (atom, s') => SOME (Atom atom, s')

end

(*

Sexpr -> atom .
Sexpr -> ( SexprList ) .

SexprList -> Sexpr SexprList .
SexprList -> .

*)

structure Parser =
struct

exception Error

datatype sexpr = Atom of string | List of sexpr list

(* given a token reader, produce an sexpr (AST) reader *)
fun parse (rdr : (Lexer.token, 'a) reader) : (sexpr, 'a) reader =
    let
       fun parseSexpr s =
           case rdr s of
               SOME (Lexer.Atom a, s') => SOME (Atom a, s')
             | SOME (Lexer.LParen, s') => parseSexprList s' []
             | SOME (Lexer.RParen, _)  => NONE
             | NONE => NONE

       and parseSexprList s acc =
           case rdr s of
               NONE => NONE
             | SOME (Lexer.RParen, s') => SOME (List (rev acc), s')
             | SOME _ => case parseSexpr s of
                             SOME (x, s') => parseSexprList s' (x :: acc)
                           | _ => NONE
    in
       parseSexpr
    end

end

fun list s =
   case s of
       []      => NONE
     | x :: xs => SOME (x, xs)


local
   open Parser
   val lex = Lexer.tokenize list
   val parse = (parse lex) o String.explode
in
   val SOME (Atom "foo", [])                           = parse "foo"
   val SOME (List [], [])                              = parse "()"
   val SOME (List [Atom "foo"], [])                    = parse "(foo)"
   val SOME (List [Atom "foo", Atom "bar"], [])        = parse "(foo bar)"
   val SOME (List [Atom "foo", List [Atom "bar"]], []) = parse "(foo (bar))"
end
