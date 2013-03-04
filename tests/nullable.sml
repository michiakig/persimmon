structure SimpleTests =
struct

structure TermSpec=(* : TERM_SPEC = *)
   struct
      datatype nonterm = X | Y | Z
      datatype term = d | c | a

      fun compareNonTerm (X, Y) = LESS
        | compareNonTerm (X, Z) = LESS
        | compareNonTerm (Y, Z) = LESS
        | compareNonTerm (Y, X) = GREATER
        | compareNonTerm (Z, X) = GREATER
        | compareNonTerm (Z, Y) = GREATER
        | compareNonTerm _ = EQUAL

      structure NonTermKey: ORD_KEY =
         struct
            type ord_key = nonterm
            val compare = compareNonTerm
         end

      fun compareTerm (d, c) = LESS
        | compareTerm (d, a) = LESS
        | compareTerm (c, a) = LESS
        | compareTerm (c, d) = GREATER
        | compareTerm (a, d) = GREATER
        | compareTerm (a, c) = GREATER
        | compareTerm _ = EQUAL

      structure TermKey: ORD_KEY =
         struct
            type ord_key = term
            val compare = compareTerm
         end

      datatype symbol = Term of term | NonTerm of nonterm

      fun showNonTerm X = "X"
        | showNonTerm Y = "Y"
        | showNonTerm Z = "Z"

      fun showTerm d = "d"
        | showTerm c = "c"
        | showTerm a = "a"

      type t = symbol

      fun show (Term t) = showTerm t
        | show (NonTerm nt) = showNonTerm nt

      fun eq (Term _, NonTerm _) = false
        | eq (NonTerm _, Term _) = false
        | eq (Term t1, Term t2) = (case compareTerm (t1, t2) of
                                      EQUAL => true
                                    | _ => false)
        | eq (NonTerm n1, NonTerm n2) = case compareNonTerm (n1, n2) of
                                            EQUAL => true
                                          | _ => false
   end

structure Persimmon = PersimmonFn(TermSpec)

open TermSpec
open Persimmon

(* some sugar *)
infix 9 -->
fun (x --> ys) = Prod (x, ys)
fun \ x = Term x
fun ` x = NonTerm x

(* grammar 3.12 from Appel *)
val grammar = [Z --> [\ d],
               Z --> [` X, ` Y, ` Z],
               Y --> [],
               Y --> [\ c],
               X --> [` Y],
               X --> [\ a]]

val terminals = [d, c, a]
val n = nullable grammar (* NonTerm set *)
val fi = FIRST (n, terminals, grammar) (* (Symbol, NonTerm set) map *)
val fo = FOLLOW (n, fi, grammar) (* (Symbol, NonTerm set) map *)

structure NonTermSetShow = SetShowFn(structure Set = Persimmon.NonTermSet
                                     structure Show =
                                        struct
                                           type t = TermSpec.nonterm
                                           val show = TermSpec.showNonTerm
                                        end)

structure NonTermSetEq: EQ =
   struct
      type t = Persimmon.NonTermSet.set
      val eq = Persimmon.NonTermSet.equal
   end

local
   structure NullableTest = TestFn(structure Show = NonTermSetShow
                                   structure Eq = NonTermSetEq)
   open NullableTest

   structure S = Persimmon.NonTermSet
   structure M = Persimmon.SymMap

   val nullable = S.addList(S.empty, [X,Y])

   val nullableTests =
       TGroup ("simple (3.12)",
               [Case ("nullable", {actual=n, expect=nullable})
                (* Case ("FIRST(X)", {actual= Option.valOf (M.find (fi, NonTerm X))} *)
              ])
in
   fun doTests (_,_) = (runTests true nullableTests; 1)
end
