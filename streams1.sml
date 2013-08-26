

signature STREAM =
sig
   type 'a t (* stream consisting of elements of type 'a *)
   val fromList : 'a list -> 'a t
   val fromIO : TextIO.StreamIO.instream -> char t

   val next : 'a t -> ('a * 'a t) option (* StringCvt.reader *)
   val isEmpty : 'a t -> bool
   val peek : 'a t -> 'a option
end

structure Stream =
struct

type 'a t = List of 'a list

fun fromList l = List l

fun next (List []) = NONE
  | next (List (x::xs)) = SOME (x, List xs)

fun isEmpty (List []) = true
  | isEmpty (List _) = false

fun peek (List []) = NONE
  | peek (List (x::xs)) = SOME x

end
