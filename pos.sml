structure Pos :>
sig
   (* Pos.t is an opaque type representing position in a stream *)
   (* ??? should include raw char count as well as line and col ??? *)
   type t

   (* Pos.zero is the start of a file: line 1, column 0 *)
   val zero: t

   (* Select fields *)
   val col: t -> int
   val line: t -> int

   (* Increment fields *)
   val incrCol: t -> t
   val incrLine: t -> t

   (* Wrap a stream with the zero position *)
   val stream: 'a -> 'a * t

   (* Given a char reader, return a positional char reader *)
   val reader: (char, 'a) StringCvt.reader -> (char * t, 'a * t) StringCvt.reader

   val show: t -> string
end =
struct
   type t = {col: int, line: int}
   val zero = {col = 0, line = 1}

   fun col      {col, line} = col
   fun line     {col, line} = line

   fun incrCol  {col, line} = {col = col + 1, line = line}
   fun incrLine {col, line} = {col = 0,       line = line + 1}

   fun stream s = (s, zero)

   fun reader rdr =
       fn (s, p) =>
          case rdr s of
              NONE            => NONE
            | SOME (#"\n", t) => SOME ((#"\n", p), (t, incrLine p))
            | SOME (x,     t) => SOME ((x,     p), (t, incrCol  p))

   fun show {col, line} = Int.toString line ^ ":" ^ Int.toString col
end

(*
local
   open Pos
in

val 0 = col  zero
val 1 = line zero

val 1 = col  (incrCol  zero)
val 2 = line (incrLine zero)

val 0 = col  (incrLine zero)
val 0 = col  (incrLine (incrCol zero))

val rdr = reader Substring.getc
val SOME ((#"f", p), (_, pp)) = rdr (stream (Substring.full "foo"))
val 0 = col  p
val 1 = line p
val 1 = col  pp
val 1 = line pp

val SOME (_, s) = rdr (stream (Substring.full "x\ny"))
val SOME ((#"\n", p), s as (_, pp)) = rdr s
val 1 = col  p
val 1 = line p

val 2 = line pp
val SOME ((#"y", p), s as (_, pp)) = rdr s
val 0 = col  p
val 2 = line p

end
*) 
