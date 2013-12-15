structure Reader =
struct

(* The type of readers: effectively a `peek` function:
   function from stream to option of element and stream *)
type ('a, 'b) t = 'b -> ('a * 'b) option

(* Reader for lists *)
val list : ('a, 'a list) t =
    fn [] => NONE
  | (x::xs) => SOME (x, xs)

local
   open String
in
   (* Reader for strings -- note this is inefficient, use substring instead *)
   val string : (char, string) t =
    fn "" => NONE
     | s => SOME (sub (s, 0), substring (s, 1, size s - 1))
end

local
   open Substring
in
   (* Reader for substrings -- peek should be O(1) time & space? *)
   val substring : (char, substring) t = getc
end

(* Reader for I/O *)
val streamIO : (char, TextIO.StreamIO.instream) t = TextIO.StreamIO.input1

(* Map over a stream: given a function f, and a reader, returns a new
   reader that applies f to each elment before returning it *)
fun map (f : 'a -> 'c) (rdr : ('a, 'b) t) : ('c, 'b) t =
    fn s =>
       case rdr s of
           NONE => NONE
         | SOME (x, s') => SOME (f x, s')

(* Filter a stream: given a predicate p, and a reader, returns a new
   reader that returns only elements x in the stream where (p x) is true *)
fun filter (p : 'a -> bool) (rdr : ('a, 'b) t) : ('a, 'b) t =
    let
       fun rdr' s =
           case rdr s of
               NONE => NONE
             | SOME (x, s') =>
               if p x then
                  SOME (x, s')
               else rdr' s'
    in
       rdr'
    end

(* Fold left over a stream: consumes the stream and reduces it *)
fun foldl (f : 'a * 'c -> 'c) (init : 'c) (rdr : ('a, 'b) t) (s : 'b) : 'c =
    let
       fun foldl' acc s =
           case rdr s of
               NONE => acc
             | SOME (x, s') => foldl' (f (x, acc)) s'
    in
       foldl' init s
    end

(* Utility: given a reader and a stream, consume the entire stream and return a
   list of the resulting elements *)
fun consume (rdr : ('a, 'b) t) s =
    let
       fun consume' acc s =
           case rdr s of
               NONE => rev acc
             | SOME (x, s') => consume' (x::acc) s'
    in
       consume' [] s
    end

end
