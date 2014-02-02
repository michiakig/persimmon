(* Lexer and parser for simplified s-expressions (no dotted-pairs) *)

type ('a, 'b) reader = 'b -> ('a * 'b) option

structure Lexer =
struct

datatype token = LParen | RParen | Atom of string

fun show LParen   = "LParen"
  | show RParen   = "RParen"
  | show (Atom a) = "Atom(" ^ a ^ ")"

(*
 * Given a (positional) char reader and (positional) stream, try to extract a Scheme atom
 * (string) from the stream, and return it with the rest of the stream
 *)
fun getAtom (rdr : ((char * Pos.t), 'b) reader, s : 'b) : (string * 'b) option =
    let
       fun done [] _ = NONE
         | done acc s = SOME (String.implode (rev acc), s)

       fun getAtom' acc s =
           case rdr s of
                NONE => done acc s
              | SOME ((#"(", _), rest) => done acc s
              | SOME ((#")", _), rest) => done acc s
              | SOME ((x,    _), rest) =>
                if Char.isSpace x then
                   done acc s
                else getAtom' (x :: acc) rest
    in
       getAtom' [] s
    end

(*
 * Given a (positional) char reader, produce a (positional) token reader
 *)
fun makeLexer (rdr : (char * Pos.t, 'a) reader) : (token * Pos.t, 'a) reader =
    let
       fun skipWS rdr s =
           case rdr s of
               NONE => s
             | SOME ((x, _), s') => if Char.isSpace x then skipWS rdr s' else s
    in
       fn s =>
          case rdr (skipWS rdr s) of
              NONE => NONE
            | SOME ((#"(", p), s') => SOME ((LParen, p), s')
            | SOME ((#")", p), s') => SOME ((RParen, p), s')
            | SOME ((_, p), s') =>
              case getAtom (rdr, skipWS rdr s) of
                  NONE => NONE
                | SOME (atom, s') => SOME ((Atom atom, p), s')
    end

end

(*

Sexpr -> atom .
Sexpr -> ( SexprList ) .

SexprList -> Sexpr SexprList .
SexprList -> .

*)

structure Parser =
struct

datatype 'a sexpr = Atom of 'a * string | List of 'a * 'a sexpr list

datatype ('a,'b) result = EOF | Error of string * 'b | Success of 'a * 'b
type ('a,'b) parser = 'b -> ('a,'b) result

(* given a token reader, produce an sexpr (AST) parser *)
fun makeParser (rdr : (Lexer.token * Pos.t, 'a * Pos.t) reader) : (Pos.t sexpr, 'a * Pos.t) parser =
    let
       val debug = false

       fun log msg s =
           if debug then
              (print msg
              ; print ":"
              ; print (case rdr s of
                           SOME ((t, _), _) => (Lexer.show t)
                         | NONE             => "eof")
              ; print "\n")
           else ()

       fun sexpr s =
           (log "sexp" s;
            case rdr s of
                SOME ((Lexer.Atom a, p), s') => Success (Atom (p, a), s')
              | SOME ((Lexer.LParen, p), s') => sexprList p s' []
              | SOME ((Lexer.RParen, _), _)  => Error ("unexpected )", s)
              | NONE => EOF)

       and sexprList p s acc =
           (log "sexpList" s;
            case rdr s of
                NONE                         => Error ("unexpected EOF", s)
              | SOME ((Lexer.RParen, _), s') => Success (List (p, rev acc), s')
              | SOME _                       =>
                case sexpr s of
                    Success (x, s') => sexprList p s' (x :: acc)
                  | other           => other)
    in
       sexpr
    end

end

local
   fun getc "" = NONE
     | getc s  = SOME (String.sub (s, 0), String.substring (s, 1, size s - 1))

   val getc = Pos.reader getc

in
   val lex   = Lexer.makeLexer getc
   val parse = Parser.makeParser lex
end

local
   open Lexer
in
   val SOME ((Atom "foo", {col=0, line=1}),
             ("", {col=3, line=1})) = lex (Pos.stream "foo")

   val SOME ((Atom "bar", {col=3, line=2}),
             ("", {col=6, line=2})) = lex ("bar", {col=3, line=2})

   val NONE = lex (Pos.stream "")
end

local
   open Parser
in
   val Success (List ({line=1, col=0},
                      [Atom ({line=1, col=1}, "foo"),
                       List ({line=2, col=0},
                             [Atom ({line=2, col=1}, "bar")])]),
                _) =
       parse (Pos.stream "(foo\n(bar))")
end
