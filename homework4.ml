(*************************************************** 

   HOMEWORK 4

   Name: Jennifer Wei

   Email: jennifer.wei@students.olin.edu

   Remarks, if any:
   - is there a less sketchy way of getting around the pattern matching is not exhaustive warning? 
   - I may have mentioned this on a previous hw, but I'm still a bit confused about functions and when to put parentheses around inputs and when not to
   - prefixes is SUPER sketchy, where I accidentally got suffixes to work first and just modified that until prefixes worked
   - received some help with Q3 from Sophie about how to approach
   - received some help from Nitya on permutations and how to think about it (it's so nested - was a bit tough to think about)


 ***************************************************)



(*
*
* Please fill in this file with your solutions and submit it
*
* The functions below are stubs that you should replace with your
* own implementation.
*
* Always make sure you can #use this file before submitting it.
* Do that in a _fresh_ OCaml shell 
* It has to load without any errors.
*
*)


(* 
* String <-> characters utility functions:
*
*   explode : string -> char list
*      returns the list of characters making up a string
*
*   implode : char list -> string
*      concatenates the list of characters into a string
*
*)

let explode str = 
  let rec acc index result = 
    if (index<0) then result
    else acc (index-1) ((String.get str index)::result) in
    acc (String.length(str)-1) []

let implode cs = 
  List.fold_right (fun a r -> (String.make 1 a)^r) cs ""



(*
*  The type of a DETERMINISTIC finite automaton
* 
*  Note that the delta here is _actually_ a function
*  from states and symbols to states
* 
*)

type 'a dfa = { states: 'a list;
                alphabet: char list;
                delta: 'a -> char -> 'a;
                start : 'a;
                accepting : 'a list }


(*
*  A sample DFA that accepts all strings over
*  {a,b} with a multiple-of-3 number of a's
*
*)

let dfaThreeA = { 
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = (fun q a -> 
            match (q,a) with
              | ("start",'a') -> "one"
              | ("one",'a') -> "two"
              | ("two",'a') -> "start"
              | ("start",'b') -> "start"
              | ("one",'b') -> "one"
              | ("two",'b') -> "two"
              | (_,_) -> "meh"); (*added this b/c it wouldn't compile otherwise*)
  start = "start";
  accepting = ["start"]
} 



(* QUESTION 1 *)


(* takes a dfa and state and returns true when h is an accepting state, false otherwise *)
let isAccepting dfa s = List.fold_right (fun h acc -> if (h=s) then true else acc) dfa.accepting false ;;

(* isAccepting tests *)
(**
   isAccepting dfaThreeA "start";;
   isAccepting dfaThreeA "one";;
   isAccepting dfaThreeA "two";;
 **)


(* takes a dfa, state, and list of symbols returns state resulting from transition of dfa from q according to symbols *)
let steps dfa q syms = List.fold_right (fun l acc -> dfa.delta acc l) syms q;;

(* steps tests *)
(**
   steps dfaThreeA "start" [];;
   steps dfaThreeA "start" ['a'];;
   steps dfaThreeA "start" ['a';'b'];;
   steps dfaThreeA "start" ['a';'b';'a'];;
   steps dfaThreeA "one" [];;
   steps dfaThreeA "one" ['a'];;
   steps dfaThreeA "one" ['a';'b'];;
   steps dfaThreeA "one" ['a';'b';'a'];;
 **)


(* takes a dfa and string and returns true if m accepts s, false otherwise *)
let acceptDFA dfa input = let exploded = explode input in isAccepting dfa (steps dfa dfa.start exploded);;

(* acceptDFA tests *)
(**
   acceptDFA dfaThreeA "";;
   acceptDFA dfaThreeA "a";;
   acceptDFA dfaThreeA "b";;
   acceptDFA dfaThreeA "aa";;
   acceptDFA dfaThreeA "aaa";;
   acceptDFA dfaThreeA "ababa";;
   acceptDFA dfaThreeA "abababa";;
 **)


(* This function loops through all the strings
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is basically the same as in the last homework
*)

let langDFA dfa n = 
  let rec expt a n = if n <= 0 then 1 else a*(expt a (n-1)) in
  let rec take n default l = 
    if n <= 0 then []
    else (match l with
           | [] -> default::(take (n-1) default l)
           | x::xs -> x::(take (n-1) default xs)) in
  let to_base_n base size n = 
    let rec loop n = 
      if n <= 0 then []
      else if n mod base = 0 then 0::(loop (n / base))
      else (n mod base)::(loop ((n - n mod base) / base))  in
      take size 0 (loop n)  in
  let to_string alphabet size n = 
    let base = List.length alphabet in
    let num_base = to_base_n base size n in
      implode (List.map (fun i -> List.nth alphabet i) num_base) in
    if n < 0 then ()
    else
      let print_str s = if s = "" then print_string "  <epsilon>\n"
        else print_string ("  "^s^"\n")  in
      let rec loop i = 
        if i <= n then 
          let ts = to_string dfa.alphabet i  in
          let bound = expt (List.length dfa.alphabet) i in
          let rec loop2 j = 
            if j < bound then (if acceptDFA dfa (ts j) 
                               then print_str (ts j)
                               else ();
                               loop2 (j+1))
            else ()  in
            (loop2 0; loop (i+1))
        else ()  in
        loop 0



(* QUESTION 2 *)
(** used http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for reference**)

(* returns true if at least n elements in xs satisfy p *)
let at_least n p xs = 
  if List.length (List.find_all p xs) >= n then true 
  else false
;;

(* at_least tests *)
(**
   at_least 0 (fun x -> x) [];;
   at_least 1 (fun x -> x) [];;
   at_least 0 (fun x -> x) [true;true;false];;
   at_least 1 (fun x -> x > 0) [2;3;0];;
   at_least 2 (fun x -> x > 0) [2;3;0];;
   at_least 3 (fun x -> x > 0) [2;3;0];;
 **)


(* returns max positive el in list if one exists 0 otherwise *)
let max_positive xs =  List.fold_right (fun h acc -> if (h>acc) then h else acc) xs 0;;

(* max_positive tests *)
(**
   max_positive [];;
   max_positive [4];;
   max_positive [4;5];;
   max_positive [5;4];;
   max_positive [4;6;5];;
   max_positive [-1;-2;-3];;
 **)


(* List.map f [a1;...;an] applies f to a1,...,an and builts results list *)
let map_funs fs x =  List.map (fun f -> f x) fs;;

(* map_fun tests *)
let dbl x = "double of "^(string_of_int x);;
let neg x = "negation of "^(string_of_int x);;
map_funs [] 3;;
map_funs [dbl] 3;;
map_funs [dbl;neg] 3;;
map_funs [dbl;neg;dbl] 3;;
map_funs [(fun x -> x * 2); (fun x -> x * x)] 10;;
map_funs [(fun x -> "+"^x); (fun x -> "-"^x)] "hello";;


(* returns all the results of applying a function in fs to a value in xs *)
let map_cross fs xs = List.fold_right (fun h acc -> (map_funs fs h) @ acc) xs [];;

(* map cross tests *)
(**
   map_cross [] [];;
   map_cross [] [1;2;3];;
   map_cross [dbl; neg] [];;
   map_cross [dbl] [3];;
   map_cross [dbl] [1;2;3];;
   map_cross [dbl;neg] [3];;
   map_cross [dbl;neg] [1;2;3];;
   map_cross [(fun x -> "+"^x);(fun x -> "-"^x)] ["hello";"world"];;
 **)

(* goes through all the xs using fold_right and uses map to create the pairs for each x and the ys, appending all the results into a final array*)
let all_pairings xs ys = List.fold_right (fun h acc -> ((List.map (fun y -> (h,y)) ys) @ acc)) xs [];;


(* all_pairings tests *)
(**
   all_pairings [] [];;
   all_pairings [1;2] [];;
   all_pairings [] ["a";"b";"c"];;
   all_pairings [1] ["a";"b";"c"];;
   all_pairings [1;2] ["a"];;
   all_pairings [1;2] ["a";"b";"c"];;
 **)



(* QUESTION 3 *)

(* prefixes xs returns list of all prefixes of xs with empty list*)

let prefixes xs = 
  let xs = List.rev xs in
    (* use List.hd acc to grab the previous array and add to that *)
    (* this is a sketchy implementation where I got suffixes to work first 
       and just built off of that until this appeared to work *)
    List.rev (List.fold_right (fun h acc -> ((List.hd acc) @ [h]) :: acc) xs [[]])
;;

(* prefixes tests *)
(**
   prefixes [];;
   prefixes [1];;
   prefixes [1;2;3;4];;
   prefixes ["a";"b"];;
 **)



(* returns list of suffixes of xs with empty list *)
let suffixes xs =  
  (* Similar to prefixes except don't need the reverse with basecase of an empty array 
     and adding arrays in front of it*)
  List.fold_right (fun h acc -> ([h] @ (List.hd acc)) :: acc) xs [[]]
;;

(* suffixes tests *)
(**
   suffixes [];;
   suffixes [1];;
   suffixes [1;2;3;4];;
   suffixes ["a";"b";"c"];;
 **)



(* returns all ways in which value a can be added to list xs *) (* uses old functs plus map2 - hope that's ok *)
let inject a xs =
  let combine x y = x@[a]@y and pres = prefixes xs and sufs = suffixes xs in
    List.map2 combine pres sufs
;;

(* inject tests *)
(**
   inject 99 [];; (* int list list = [[99]] *)
   inject 99 [1];; (* int list list = [[99; 1]; [1; 99]] *)
   inject 99 [1;2];; (* int list list = [[99; 1; 2]; [1; 99; 2]; [1; 2; 99]] *)
   inject 99 [1;2;3;4];; (* int list list = [[99; 1; 2; 3; 4]; [1; 99; 2; 3; 4]; [1; 2; 99; 3; 4]; [1; 2; 3; 99; 4]; [1; 2; 3; 4; 99]] *)
   inject "X" ["a";"b"];; 
 **)



(* used to inject x into each subarray in acc from permutations *)
let permutationsHelper x xs = List.fold_right (fun sub acc -> (inject x sub) @ acc) xs [];;
(* returns list of all permutations of xs (different order of same elements, treat repeated elements as distinct *)
let permutations xs =  List.fold_right (fun h acc -> (permutationsHelper h acc)) xs [[]];;

(* permutations tests *)
(**
   permutations [];;
   permutations [1];;
   permutations [1;2];;
   permutations [1;2;3;4];;
   permutations ["a";"b"];;
 **)


