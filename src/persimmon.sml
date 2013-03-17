
(* specifies types for grammar symbols, terminals and non-terminals *)
signature TERM_SPEC =
   sig
      structure NonTermKey: ORD_KEY
      structure TermKey: ORD_KEY
      type nonterm = NonTermKey.ord_key
      type term = TermKey.ord_key
      datatype symbol = Term of term | NonTerm of nonterm

      val showNonTerm: nonterm -> string
      val showTerm: term -> string
      val show: symbol -> string
      val eq: symbol * symbol -> bool
   end

functor PersimmonFn(TermSpec: TERM_SPEC) =
   struct
      open TermSpec
      datatype production = Prod of nonterm * symbol list

      structure NonTermSet = ListSetFn(NonTermKey)
      structure TermSet = ListSetFn(TermKey)

      structure SymKey: ORD_KEY =
         struct
            type ord_key = symbol
            fun compare (Term t1, Term t2) = TermKey.compare(t1,t2)
              | compare (NonTerm t1, NonTerm t2) = NonTermKey.compare(t1,t2)
              | compare (NonTerm _, Term _) = GREATER
              | compare (Term _, NonTerm _) = LESS
         end

      structure SymMap = ListMapFn(SymKey)
      structure TermSetShow = SetShowFn(structure Set = TermSet
                                        structure Show =
                                           struct
                                              type t = TermSpec.term
                                              val show = TermSpec.showTerm
                                           end)

      local
         structure K =
            struct
               type t = TermSpec.symbol
               val show = TermSpec.show
            end
         structure V =
            struct
               type t = TermSet.set
               val show = TermSetShow.show
            end
      in
         structure SymMapShow = MapShowFn(structure Map = SymMap
                                          structure K = K
                                          structure V = V)
      end

      (* compute a set of NonTerms which can derive the empty string *)
      fun nullable prods =
         let

            (* given a set, returns a predicate that returns true if
            arg is a NonTerm in the set, false if it is a Term *)
            fun nullable' set =
               (fn x => case x of
                            Term _ => false
                          | NonTerm nt => NonTermSet.member (set, nt))

            fun step set =
               foldl (fn (Prod (nt, syms), acc) =>
                         if null syms orelse List.all (nullable' acc) syms
                            then NonTermSet.add (acc, nt)
                         else acc)
                     set
                     prods
         in
            HigherOrder.fixedPoint (NonTermSet.empty, step, NonTermSet.equal)
         end

      (* test two instances of FIRST or FOLLOW for equality *)
      fun eq (x,y) =
          if SymMap.numItems x <> SymMap.numItems y
          then false
          else
             let
                fun f (k,v) =
                    case SymMap.find (y, k) of
                        NONE => false
                      | SOME v' => TermSet.compare (v, v') = EQUAL
             in
                List.all f (SymMap.listItemsi x)
             end

      fun isNullable nullable (Term _) = false
        | isNullable nullable (NonTerm nt) = NonTermSet.member (nullable, nt)

      (* given a FIRST map, insert x and y into the map, merging as
      necessary *)
      fun insertIntoFirst (first,x,y) =
          let
             val firstx = SymMap.find (first,x)
             val firsty = SymMap.find (first,y)
          in
             case (firstx, firsty) of
                 (_, NONE) => first
               | (NONE, SOME y') => SymMap.insert (first, x, y')
               | (SOME x', SOME y') =>
                 SymMap.insert (first, x, TermSet.union (x', y'))
          end

      (* compute FIRST sets for all grammar symbols as a map from
      symbols to sets of non-terminals *)
      fun FIRST (nullable, terminals: term list, prods) =
         let
            fun f (x,acc) = SymMap.insert(acc,Term x,TermSet.singleton x)
            val init = foldl f SymMap.empty terminals

            fun forEachProd (Prod (x,ys), first) =
                case (foldl (fn (sym, (wasNull, first)) =>
                                let val first' =
                                        if wasNull
                                        then insertIntoFirst (first, NonTerm x, sym)
                                        else first
                                in (wasNull andalso isNullable nullable sym, first')
                                end)
                            (true,first)
                            ys) of
                    (_,first') => first'

            fun step first = foldl forEachProd first prods

         in
            HigherOrder.fixedPoint (init, step, eq)
         end

      fun insertIntoFollow (first,follow,y,x) =
          let
             val firstx = SymMap.find (first,x)
             val followy = SymMap.find (follow,y)
          in
             case (firstx,followy) of
                 (NONE, _) => follow
               | (SOME x', NONE) => SymMap.insert (follow, y, x')
               | (SOME x', SOME y') =>
                 SymMap.insert (follow, y, TermSet.union (x', y'))
          end


      fun FOLLOW (nullable, first, prods) =
          let
             val init = SymMap.empty

             val g = fn Yi =>
                        fn (ys, acc) =>
                           if null ys then acc else
                 let
                    val len = length ys
                    (* Yi+1...Yj-1 *)
                    val ys' = List.take (ys, len - 1)

                    val s = (map TermSpec.show ys')
                    val _ = print "  ";
                    val _ = print "Yi+1...Yj-1=";
                    val _ = List.app (fn x => (print x; print " ")) s
                    val _ = print "\n";

                    val Yiplus1 = hd ys
                    val Yj = List.last ys
                 in
                    (* if Yi+1...Yj-1 all nullable, FOLLOW(Yi) U= FIRST(Yj) *)
                    if List.all (isNullable nullable) ys'
                    then insertIntoFollow (first, acc, Yi, Yj)
                    else acc
                 end

             fun forEachProd (Prod (x,ys), follow) =
                 let
                    fun f ([], acc) = acc
                      | f (ys, acc) =
                        let
                           val _ = print ("Yi=" ^ (TermSpec.show (hd ys)) ^ ",")
                           val s = (map TermSpec.show (tl ys))
                           val _ = print "Yi+1...k=";
                           val _ = List.app (fn x => (print x; print " ")) s
                           val _ = print "\n";
                           val acc' =
                               (* if Yi+1...Yk all nullable, FOLLOW(Yi) U= FOLLOW(x) *)
                               if List.all (isNullable nullable) (tl ys)
                               then insertIntoFirst (acc, (hd ys), NonTerm x)
                               else acc
                        in
                           ExtList.foldtake (g (hd ys), acc', tl ys)
                        end
                 in
                    ExtList.folddrop (f, follow, ys)
                 end

             fun step follow =
                 (print (SymMapShow.show follow);
                  print "\n";
                  foldl forEachProd follow prods)

          in
             HigherOrder.fixedPoint (init, step, eq)
          end
   end
