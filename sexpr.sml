(* Lexer and parser for simplified s-expressions (no dotted-pairs) *)

type ('a, 'b) reader = 'b -> ('a * 'b) option

structure Lexer =
struct

datatype token = LParen | RParen | Atom of string

fun show LParen   = "("
  | show RParen   = ")"
  | show (Atom a) = a

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
fun makeLexer (rdr : (char, 'a) reader) : (token, 'a) reader =
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

datatype sexpr = Atom of string | List of sexpr list

datatype ('a,'b) result = EOF | Error of string * 'b | Success of 'a * 'b
type ('a,'b) parser = 'b -> ('a,'b) result

(* given a token reader, produce an sexpr (AST) parser *)
fun makeParser (rdr : (Lexer.token, 'a) reader) : (sexpr, 'a) parser =
    let
       val debug = false

       fun log msg s =
           if debug then
              (print msg
              ; print ":"
              ; print (case rdr s of
                           SOME (t, _) => (Lexer.show t)
                         | NONE        => "eof")
              ; print "\n")
           else ()

       fun sexpr s =
           (log "sexp" s;
            case rdr s of
                SOME (Lexer.Atom a, s') => Success (Atom a, s')
              | SOME (Lexer.LParen, s') => sexprList s' []
              | SOME (Lexer.RParen, _)  => Error ("unexpected )", s)
              | NONE => EOF)

       and sexprList s acc =
           (log "sexpList" s;
            case rdr s of
                NONE                    => Error ("unexpected EOF", s)
              | SOME (Lexer.RParen, s') => Success (List (rev acc), s')
              | SOME _                  =>
                case sexpr s of
                    Success (x, s') => sexprList s' (x :: acc)
                  | result => result)
    in
       sexpr
    end

end

local
   open Parser

   fun getc "" = NONE
     | getc s  = SOME (String.sub (s, 0), String.substring (s, 1, size s - 1))

   val lex   = Lexer.makeLexer getc
   val parse = makeParser lex
in

val Success (Atom "foo", "")                           = parse "foo"
val Success (List [], "")                              = parse "()"
val Success (List [Atom "foo"], "")                    = parse "(foo)"
val Success (List [Atom "foo", Atom "bar"], "")        = parse "(foo bar)"
val Success (List [Atom "foo", List [Atom "bar"]], "") = parse "(foo (bar))"

val Success (Atom "foo", " bar")  = parse "foo bar"
val Success (Atom "foo", ") bar") = parse "foo) bar"

val Error ("unexpected )", ") bar") = parse ") bar"
val Error ("unexpected EOF", "")    = parse "("

end
