(*
 * Some functions for working with TextIO.StreamIO
 *)
structure StreamUtils = struct
local
   structure S = TextIO.StreamIO
in

fun length s =
    case S.input1 s of
        NONE => 0
      | SOME (_, s') => 1 + length s'

fun input1If (p : S.elem -> bool) (s : S.instream) : (S.elem * S.instream) option =
    case S.input1 s of
        NONE => NONE
      | SOME (c, s') => if p c then SOME (c, s') else NONE

fun inputWhile (p : S.elem -> bool) (s : S.instream) : S.vector * S.instream =
    let
       fun inputWhile' acc s =
           case input1If p s of
               NONE => (String.implode (rev acc), s)
             | SOME (c, s') => inputWhile' (c :: acc) s'
    in
       inputWhile' [] s
    end

val fromFile : string -> TextIO.StreamIO.instream =
    TextIO.getInstream o TextIO.openIn

val fromStr : string -> TextIO.StreamIO.instream =
    TextIO.getInstream o TextIO.openString

end
end
