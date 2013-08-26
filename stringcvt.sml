
val listReader : ('a, 'a list) StringCvt.reader =
    fn [] => NONE
  | (x::xs) => SOME (x, xs)

local
   open String
in
   val stringReader : (char, string) StringCvt.reader =
    fn "" => NONE
     | s => SOME (sub (s, 0), substring (s, 1, size s - 1))
end

val ioReader : (char, TextIO.StreamIO.instream) StringCvt.reader =
    TextIO.StreamIO.input1

fun map (f : 'a -> 'c) (rdr : ('a, 'b) StringCvt.reader) : ('c, 'b) StringCvt.reader =
    fn s =>
       case rdr s of
           NONE => NONE
         | SOME (x, s') => SOME (f x, s')

fun filter (p : 'a -> bool) (rdr : ('a, 'b) StringCvt.reader) : ('a, 'b) StringCvt.reader =
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

fun foldl (f : 'a * 'c -> 'c) (init : 'c) (rdr : ('a, 'b) StringCvt.reader) (s : 'b) : 'c =
    let
       fun foldl' acc s =
           case rdr s of
               NONE => acc
             | SOME (x, s') => foldl' (f (x, acc)) s'
    in
       foldl' init s
    end
