let rec separate (lst) =
  match lst with
    | [] -> ([],[])
    | (a,b)::t -> 
        (
          match separate (t) with
            |(l1,l2) -> (a::l1, b::l2)
        )
;;

separate [(1,2)];;
separate [(1,2);(3,4)];;
separate [(1,"a");(2,"b");(3,"c")];;

(**
   regex;;
   use egrep to match;;
   '^a' means has to start w/ a
   'a$'returns stuff w/ a as last character
   'a+b' is or
   'a|b' is and
   '^a&$' is only stuff with a's
   '^a*b*$' returns only things with a's in front b's in back
   '^(ab)*$' returns things with ab's
   '(^a*$)|(^b*$)' returns all a's or all b's
   '^(aa|bb)*$' returns things with pairs (i.e. aabbaabbaabbbbaa)
   '^a{6}' returns things that start with 6 a's
   '^a{3,6}' returns things with 3-6 a's
   '[]' is a big or statement ([abc] same as a|b|c)
   '^[ab]*$' only returns things with a and b (no others)
   '^[^c]*$' second carrot means 'not' -> ^ in ocaml means concat -> would return anything without c's

 **)

(**
   Practice Problems
   1. even number of a's that exist
   egrep "^(b*ab*ab*)*$"

   2. only with a's and c's no b's
 **)
