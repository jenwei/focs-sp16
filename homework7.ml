(* 

HOMEWORK 7

Name: Jennifer Wei

Email: jennifer.wei@students.olin.edu

Remarks, if any:
- is there a good way for debugging grammar? for this assignment, I 
would trace through the rules once I saw a 'false', but is there a better 
way to debug?
- for eqnum "dedeeeeddd", it seems like it'd work if given a large depth/width? 

*)


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
* Type for grammars
*
*)

type grammar = {
  nonterminals: string list;
  terminals: string list;
  rules: (string * string) list;
  startsym : string
}


(* 
* Some sample (context-free) grammars 
*
*)

let anbn = {
  nonterminals = ["S"];
  terminals = ["a";"b"];
  rules = [("S","");
           ("S","aSb")];
  startsym = "S"
}

let anbm = {
  nonterminals = ["S";"T";"B"];
  terminals = ["a";"b"];
  rules = [ ("S","TB");
            ("T","");
            ("T","aTb");
            ("B","");
            ("B","Bb")];
  startsym = "S"
}


(*
* Here's a grammar that is _not_ context-free
*
* It's also harder to generate its strings
*
*)

let anbncn = {
  nonterminals = ["S";"A";"B";"C";"X"];
  terminals = ["a";"b";"c"];
  rules = [ ("S","");
            ("S","ABC");
            ("bX","Xb");
            ("AX","a");
            ("aX","Xa");
            ("XC","c");
            ("Xc","cX");
            ("B","XbBX");
            ("B","");
            ("A","AA");
            ("C","CC")];
  startsym = "S"
}




(* abbreviations *)

let map = List.map
let len = String.length
let sub = String.sub


(*
* Utility functions 
* 
*)


(* check is lhs is a prefix of str *)

let prefix lhs str =
  lhs = (sub str 0 (len lhs))


(* replace prefix lhs of str with rhs *)

let replace lhs str rhs =
  let l = len lhs in
    rhs ^ (sub str l (len str - l))


(* try to apply rule (lhs,rhs) to str (assuming prefix prf) *)

let apply_rule prf (lhs,rhs) str =
  if len str < len lhs 
  then []
  else if prefix lhs str
  then [prf^(replace lhs str rhs)]
  else []


(* try to apply every rule in rs to str *)

let rec apply_rules rs str =
  let rec loop prefix str = 
    if str = "" then []
    else let rest = loop (prefix^(sub str 0 1)) (sub str 1 (len str -1))  in
        (List.fold_left (fun res r -> res@(apply_rule prefix r str)) [] rs)@rest  in
    loop "" str


(*
* Perform an iteratively deepening depth-first search of the rewrite 
* tree
*
*)

module StringSet = Set.Make(String)

let dfs_path maxdepth maxwidth grammar target =
  let lt = len target  in
  let rec loop q seen =
    if q = []
    then []
    else let ((path,d)::q) = q in
      let (str::_) = path in
        if len str > maxwidth
        then loop q seen
        else if len str = lt && str = target
        then path
        else if StringSet.mem str seen
        then loop q seen
        else if d > maxdepth
        then loop q (StringSet.add str seen)
        else (* let _ = (print_string str; print_newline()) in *)
          let new_strs = apply_rules grammar.rules str in
          let new_strs_d = map (fun x -> (x::path,d+1)) new_strs in
          let q = (new_strs_d)@q in
            loop q (StringSet.add str seen) in
    loop [([grammar.startsym],0)] StringSet.empty

let idfs_path maxdepth grammar target =
  let rec loop n =
    let _ = Printf.printf "Searching (depth %02d, max width %d)" n n in
    let _ = print_newline ()  in
      if n > maxdepth
      then []
      else match dfs_path n n grammar target with
        | [] -> loop (n+1)
        | path -> path  in
    loop 1


(* 
* Check if a grammar is well-formed 
*
*)

let checkGrammar grammar = 
  let _ = List.iter (fun x -> if String.length x != 1 then failwith ("symbol "^x^" not a single character") else ()) grammar.nonterminals  in
  let _ = List.iter (fun x -> if String.length x != 1 then failwith ("symbol "^x^" not a single character") else ()) grammar.terminals  in
  let _ = List.iter (fun (p,q) -> if String.length p < 1 then failwith "rule with empty left-hand side" else ()) grammar.rules  in
  let _ = if List.mem grammar.startsym grammar.nonterminals then () else failwith "start symbol not a nonterminal"  in
    ()



(*
* Try to generate a string for a given grammar 
* 
*)

let generate md grammar str =
  let _ = checkGrammar grammar in
  let print pre str = (print_string pre;
                       print_string str;
                       print_newline ())  in
  let rec rev_print path =
    match path with
      | [] -> ()
      | [s] -> print "   " s
      | s::ss -> (rev_print ss; print "-> " s)  in
  let path = idfs_path md grammar str  in
  let _ = rev_print path  in
    path != []



(* 
* QUESTION 1
*
*)

let palindromes = {
  nonterminals = ["S"];
  terminals = ["a";"b";"c"];
  rules = [
    ("S","");
    ("S","a");
    ("S","b");
    ("S","c");
    ("S","aSa");
    ("S","bSb");
    ("S","cSc")
  ];
  startsym = "S"
};;

(** test palindromes **)
(*
generate 10 palindromes "aba";;
generate 10 palindromes "aa";;
generate 10 palindromes "abba";;
generate 10 palindromes "abcacba";;
*)


let ambncmn = {
  nonterminals = ["S";"T"];
  terminals = ["a";"b";"c"];
  rules = [
  	("S","");
  	("S","aSc");
  	("S","T");
  	("T","bTc");
  	("T","S");
  	("T","")
  ];
  startsym = "S"
};;

(** test ambncmn **)
(*
generate 10 ambncmn "aabbcccc";;
generate 20 ambncmn "aabbbccccc";;
generate 10 ambncmn "abcc";;
generate 20 ambncmn "aaabbccccc";;
*)


let amcmnbn = { 
  nonterminals = ["S";"T"];
  terminals = ["a";"b";"c"];
  rules = [
  	("S","");
  	("S","aScT");
  	("T","cTb");
  	("T","")
  ];
  startsym = "S"
};;

(** test amcmnbn **)
(* 
generate 20 amcmnbn "accb";;
generate 20 amcmnbn "aacccb";;
generate 20 amcmnbn "acccbb";;
generate 20 amcmnbn "aaccccbb";;
generate 20 amcmnbn "acb";; // false
*)


let ambncm = {
  nonterminals = ["S";"T"];
  terminals = ["a";"b";"c"];
  rules = [
  	("S","");
  	("S","aSc");
  	("S","T");
  	("T","bT");
  	("T","S");
  	("T","")
  ];
  startsym = "S"
};;

(** test ambncm **)
(*
generate 20 ambncm "aaabccc";;
generate 20 ambncm "aaabbccc";;
generate 20 ambncm "aabcc";;
generate 20 ambncm "aaabccc";;
generate 20 ambncm "aabbc";;
*)


let eqnum = {
  nonterminals = ["S"];
  terminals = ["d";"e"];
  rules = [
  	("S","");
  	("S","dSe");
  	("S","eSd")
  ];
  startsym = "S"
};;

(** test eqnum **)
(*
generate 20 eqnum "eedd";;
generate 20 eqnum "eded";;
generate 20 eqnum "ddee";;
generate 20 eqnum "dedede";;
generate 40 eqnum "eedded";;
generate 20 eqnum "dededed";; (*false*)
*)



(* 
* QUESTION 2 
*
*)

(* Type for DFAs *)

type 'a dfa = { states: 'a list;
                alphabet: char list;
                delta: 'a -> char -> 'a;
                start : 'a;
                accepting : 'a list }


(* A dfa that accepts all strings with a multiple of three
 * number of as *)

let dfaThreeA = { 
  states = ["S";"1";"2"];
  alphabet = ['a';'b'];
  delta = (fun q a -> 
            match (q,a) with
                ("S",'a') -> "1"
              | ("1",'a') -> "2"
              | ("2",'a') -> "S"
              | ("S",'b') -> "S"
              | ("1",'b') -> "1"
              | ("2",'b') -> "2"
              | _ -> "");
  start = "S";
  accepting = ["S"]
} 



let dfaGrammar dfa = failwith "dfaGrammar not implemented"



(*
* QUESTION 3 
*
*)


let addition = {
  nonterminals = [];
  terminals = [];
  rules = [];
  startsym = ""
} 


let powers2 = {
  nonterminals = [];
  terminals = [];
  rules = [];
  startsym = ""
} 
