(* 

HOMEWORK 3

Name: Jennifer Wei

Email: jennifer.wei@students.olin.edu

Remarks, if any:
- My compiler was super unhappy with the use of String.create and String.set in implode
so I had to switch it to Bytes - weird
- I don't fully understand how steps works - I knew the general structure that/format it needed to be in and tested until the test cases worked (will follow up with others to resolve my confusion)
- Got help from Dennis with how to approach isDFA (had issues figuring how what helper functions I'd need
- I've been using a lot of parentheses to help me read the code better, but apparently parentheses make a different? Didn't get a chance to clarify with the NINJAs about what they meant with that
- Got help from Kyle on Q3 stepAll (seemed like I could just reuse Q1 code, but I kept getting errors when I ran the tests)
*)


(*
*
* Please fill in this file with your solutions and submit it
*
* The functions below are stubs that you should replace with your
* own implementation.
*
* Always make sure you can #use this file before submitting it.
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

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])
;;

let implode (cs) = 
  let str = Bytes.create(List.length(cs)) in
  let rec loop (cs,index) = 
    match cs with
        [] -> str
      | c::cs -> (Bytes.set str index c; loop(cs,index+1))
  in
    loop(cs,0)
;;


(*
*  The type of a finite automaton
* 
*  When the transition relation is a function
*  (i.e., every p,a has q such that (p,a,q) is in 
*  delta) then this is a deterministic finite automaton  
* 
*)

type 'a fa = { states: 'a list;
               alphabet: char list;
               delta: ('a * char * 'a) list;
               start : 'a;
               accepting : 'a list }



(** Example DFAs (moved them up here so I Can run tests right after each function **)
let dfaThreeA = { 
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = [ ("start",'a',"one");
            ("one",'a',"two");
            ("two",'a',"start");
            ("start",'b',"start");
            ("one",'b',"one");
            ("two",'b',"two") ];
  start = "start";
  accepting = ["start"]
};;

let nfaLastThreeB = {
  states = [0;1;2;3];
  alphabet = ['a';'b';'c'];
  delta = [ (0,'a',0);
            (0,'b',0);
            (0,'c',0);
            (0,'b',1);
            (1,'b',2);
            (2,'b',3); ];
  start = 0;
  accepting = [3]
};;


(* QUESTION 1 *)


let rec findTransitionsHelper (d,q,a) = 
  (*go through the deltas and add to list if transitions from q and labeled a*)
  match d with
    | [] -> []
    | (x,y,z)::t -> if (x=q && y=a) then (x,y,z)::findTransitionsHelper (t,q,a) 
        else findTransitionsHelper (t,q,a)
;;

let findTransitions (fa,q,a) = findTransitionsHelper (fa.delta,q,a);;

(* findTransitions tests *)
(**
   findTransitions (dfaThreeA,"start",'a');;
   findTransitions (dfaThreeA,"start",'b');;
   findTransitions (dfaThreeA,"one",'b');;
   findTransitions (nfaLastThreeB,0,'a');;
   findTransitions (nfaLastThreeB,0,'b');;
 **)


let rec isAcceptingHelper (a,s) =
  match a with
    | [] -> false
    | h::t -> if (h=s) then true else isAcceptingHelper (t,s)
;;


let isAccepting (fa,s) = isAcceptingHelper (fa.accepting,s);;

(* isAccepting tests *)
(**
   isAccepting (dfaThreeA,"start");;
   isAccepting (dfaThreeA, "one");;
   isAccepting (dfaThreeA,"two");;
   isAccepting (nfaLastThreeB,3);;
   isAccepting (nfaLastThreeB,0);;
 **)

let rec stepHelper (d,q,a) =
  (*returns state of m resulting from the transition from q that applies*)
  match d with
    | [] -> failwith "conditions not satisfied"
    | (x,y,z)::t -> if (x=q && y=a) then z else stepHelper (t,q,a)
;;

let step (fa,q,a) = stepHelper (fa.delta,q,a);;

(* step tests *)
(**
   step (dfaThreeA, "start",'a');;
   step (dfaThreeA, "start",'b');;
   step (dfaThreeA, "one",'a');;
   step (dfaThreeA, "one",'b');;
   step (dfaThreeA, "two",'a');;
   step (dfaThreeA, "two",'b');;
 **)


let rec steps (fa,q,syms) = 
  (*check the symbols and returns the state of m resulting from following the transitions of m from q according to the symbols in l*)
  match syms with
    | [] -> q
    | h::t -> step (fa, steps(fa,q,t), h)
;;

(* steps tests *)
(**
   steps (dfaThreeA, "start", []);;
   steps (dfaThreeA, "start", ['a']);;
   steps (dfaThreeA, "start", ['a';'b']);;
   steps (dfaThreeA, "start", ['a';'b';'a']);;
   steps (dfaThreeA, "one", []);;
   steps (dfaThreeA, "one", ['a']);;
   steps (dfaThreeA, "one", ['a';'b']);;
   steps (dfaThreeA, "one", ['a';'b';'a']);;
 **)


let rec findTransitionCount (ds,q,a) = 
  (*go through the deltas and update counter if transition*)
  match ds with
    | [] -> 0
    | (x,y,z)::t -> if (x=q && y=a) then 1 + findTransitionCount (t,q,a) 
        else findTransitionCount (t,q,a)
;;


let rec oneTransition (ds,q,symbols) = 
  match symbols with 
    | [] -> true
    | h::t -> if findTransitionCount (ds,q,h) = 1 then oneTransition (ds,q,t) else false
;;


let rec isDFAHelper (ds,states,symbols) = 
  (*checks if every state and every symbol has exactly one transition*)
  match states with
    | [] -> true
    | h::t -> if oneTransition (ds,h,symbols) then isDFAHelper (ds,t,symbols) else false
;;


let isDFA (fa) = isDFAHelper (fa.delta,fa.states,fa.alphabet);;

(* isDFA test *)
(**
   isDFA (dfaThreeA);;
   isDFA (nfaLastThreeB);;
   isDFA {states=[0;1]; 
   alphabet=['a']; 
   delta=[(0,'a',1)]; 
   start=0; 
   accepting=[1]};;
 **)


let rec has (accepting,state) =
  match accepting with
    | [] -> false
    | h::t -> if h=state then true else has (t,state)
;;

let acceptDFA (fa,input) =
  if isDFA (fa) then (
    let symbols=explode(input) in has (fa.accepting,steps(fa,fa.start,symbols))
  )
  else failwith "not deterministic" 
;;

(* acceptDFA test *)
(**
   acceptDFA (dfaThreeA,"");;
   acceptDFA (dfaThreeA,"a");;
   acceptDFA (dfaThreeA,"b");;
   acceptDFA (dfaThreeA,"aa");;
   acceptDFA (dfaThreeA,"aaa");;
   acceptDFA (dfaThreeA,"ababa");;
   acceptDFA (dfaThreeA,"abababa");;
   langDFA (dfaThreeA,6);; (*this doesn't work because it doesn't know what langDFA is yet*)
 **)


(* QUESTION 2 *)

(* THESE ARE PLACEHOLDERS - THEY DEFINE EMPTY AUTOMATA *)
(* REPLACE BY YOUR OWN DEFINITIONS *)

(*no more than two consecutive bs in a row*)
let dfa_q2_a = { states = [0;1;2;3]; 
                 alphabet = ['a';'b'];
                 delta = [(0,'a',0);
                          (0,'b',1);
                          (1,'a',1);
                          (1,'b',2);
                          (2,'a',0);
                          (2,'b',3); (*dump*)
                          (3,'a',3);
                          (3,'b',3);
                         ];
                 start = 0;
                 accepting = [0;1;2]
               };;

(*no more than three consecutive letters in a row*)
let dfa_q2_b = { states = [0;1;2;3;4;5;6;10];
                 alphabet = ['a';'b'];
                 delta = [(0,'a',1); (*start of a chain*)
                          (0,'b',4); (*start of b chain*)
                          (1,'a',2);
                          (1,'b',5);
                          (2,'a',3);
                          (2,'b',5);
                          (3,'a',10); (*dump*)
                          (3,'b',4);
                          (4,'a',1);
                          (4,'b',5);
                          (5,'a',1);
                          (5,'b',6);
                          (6,'a',1);
                          (6,'b',10); (*dump*)
                          (10,'a',10);
                          (10,'b',10);
                         ];
                 start = 0;
                 accepting = [0;1;2;3;4;5;6]
               };;

(*three or more consecutives at least (opposite of 2b)*)
let dfa_q2_c = { states = [0;1;2;3;4;5;6;10];
                 alphabet = ['a';'b'];
                 delta = [(0,'a',1); (*start of a chain*)
                          (0,'b',4); (*start of b chain*)
                          (1,'a',2);
                          (1,'b',5);
                          (2,'a',3);
                          (2,'b',5);
                          (3,'a',10); (*dump*)
                          (3,'b',4);
                          (4,'a',1);
                          (4,'b',5);
                          (5,'a',1);
                          (5,'b',6);
                          (6,'a',1);
                          (6,'b',10); (*dump*)
                          (10,'a',10);
                          (10,'b',10); 
                         ];
                 start = 0;
                 accepting = [3;6;10]
               };;

(*all strings over alphabet {a,b} except those that do not end with an a
  where there are no bs in the first three symbols of the string*)
let nfa_q2_d = { states = [0;1;2;3;4;10];
                 alphabet = ['a';'b'];
                 delta = [ (0,'a',1);
                           (0,'b',10); (*ok*)
                           (1,'a',2);
                           (1,'b',10); (*ok*)
                           (2,'a',3);
                           (2,'b',10); (*ok*)
                           (3,'a',3);
                           (3,'b',4);
                           (4,'a',3);
                           (4,'b',4); (*does not meet requirement*)
                           (10,'a',10);
                           (10,'b',10);
                         ];
                 start = 0;
                 accepting = [0;1;2;3;10]
               };;


(* QUESTION 3 *)


(*takes a list of transitions and returns list of all states that those transitions lead to (the target)*)
(*use has function to check for duplicates*)
let rec keepTarget (trs) =
  match trs with
    | [] -> []
    | (x,y,z)::t -> 
        if has (keepTarget (t),z) then keepTarget (t) else z::keepTarget (t)
;;

(* keepTarget tests *)
(**
   keepTarget [];; 
   keepTarget [(1,'a',2);(1,'b',3)];;
   keepTarget [(1,'a',2);(1,'b',3);(2,'a',2)];;
   keepTarget (dfaThreeA.delta);;
   keepTarget (nfaLastThreeB.delta);;
 **)


(*takes fa and list of states qs of m and returns true if any states in qs is an accepting state*)
let rec isAcceptingAny (fa,qs) = 
  match qs with
    | [] -> false
    | h::t -> if has (fa.accepting,h) then true else isAcceptingAny (fa,t)
;;

(* isAcceptingAny tests *)
(**
   isAcceptingAny (nfaLastThreeB, []);;
   isAcceptingAny (nfaLastThreeB, [0]);;
   isAcceptingAny (nfaLastThreeB, [0;1]);;
   isAcceptingAny (nfaLastThreeB, [0;1;2]);;
   isAcceptingAny (nfaLastThreeB, [0;1;2;3]);;
   isAcceptingAny (nfaLastThreeB, [3]);;
 **)


(*takes fa list of states qs of m and symbol a of m and returns list of all states resulting from taking a transition from any state in qs following symbol a*)
(*remove dups*)

let nfaLastThreeB = {
  states = [0;2;3;4];
  alphabet = ['a';'b';'c'];
  delta = [ (0,'a',0);
            (0,'b',0);
            (0,'c',0);
            (0,'b',1);
            (1,'b',2);
            (2,'b',3); ];
  start = 0;
  accepting = [3]
} 

(*stole this from Q1*)
let rec stepHelper (ds,q,a) =
  (*returns state of m resulting from the transition from q that applies to the deltas*)
  match ds with
    | [] -> []
    | (x,y,z)::t -> if (x=q && y=a) then z::stepHelper (t,q,a) (*updated this from Q1 to con z with stepHelper*) else stepHelper (t,q,a)
;;

let stepQ3 (fa,q,a) = stepHelper (fa.delta,q,a);;

let rec stepAllHelper (fa,qs,a) =
  (*goes through all states and appends the outputs*)
  match qs with
    | [] -> []
    | h::t -> stepQ3 (fa,h,a)@stepAllHelper (fa,t,a) (*updated this from :: to @*)
;;

let rec removeDuplicates (vals) = 
  match vals with
    | [] -> []
    | h::t -> if has (t,h) then removeDuplicates (t) else h::(removeDuplicates (t))
;;

let stepAll (fa,qs,a) = removeDuplicates (stepAllHelper (fa,qs,a));;

(* stepAll tests *) 
(**
   stepAll (dfaThreeA,[],'a');;
   stepAll (dfaThreeA,["start"],'a');;
   stepAll (dfaThreeA,["start"],'b');;
   stepAll (dfaThreeA,["start";"one"],'a');;
   stepAll (dfaThreeA,["start";"one"],'b');;
   stepAll (nfaLastThreeB,[0;1],'a');;
   stepAll (nfaLastThreeB,[0;1],'b');;
 **)


let rec reverse (xs) = 
  match xs with 
    | [] -> []
    | h::t -> reverse (t) @ [h]
;;

let rec stepsAllHelper (fa,qs,syms) = 
  (*takes a DFA m, a state q of m, and a list of symbols l, and returns the state of m resulting from following the transitions of m from q according to the symbols in l*)
  match syms with 
    | [] -> qs
    | h::t -> stepAll(fa,stepsAllHelper (fa,qs,t), h)
;;

let stepsAll (fa,qs,syms) = stepsAllHelper (fa,qs,reverse(syms));; 
(*Talked to Dennis and Kyle, who helped me debug this and suggested reversing the symbols*)


(* stepsAll tests *)
(**
   stepsAll (dfaThreeA,[],[]);;
   stepsAll (dfaThreeA,[],['a']);;
   stepsAll (dfaThreeA,[],['a';'b']);;
   stepsAll (dfaThreeA,["start"],[]);;
   stepsAll (dfaThreeA,["start"],['a']);;
   stepsAll (dfaThreeA,["start"],['a';'b']);;
   stepsAll (dfaThreeA,["start"],['a';'a']);;
   stepsAll (dfaThreeA,["start";"one"],['a';'a']);;
   stepsAll (dfaThreeA,["start";"one"],['a';'a';'b']);;
   stepsAll (dfaThreeA,["start";"one"],['a';'a';'b';'a']);;
   stepsAll (nfaLastThreeB,[0;1],['a';'b';'b';'b']);;
 **)


let acceptNFA (fa,input) = 
  (*takes fa and string and returns true if m accepts s, false otherwise*)
  let charsList = explode (input) in 
    isAcceptingAny (fa,stepsAll(fa,[fa.start],charsList))
;;

(* acceptNFA tests *)
(**
  acceptNFA (dfaThreeA,"babab");;
  acceptNFA (dfaThreeA,"bababa");;
  acceptNFA (dfaThreeA,"bababab");;
  acceptNFA (nfaLastThreeB,"abb");;
  acceptNFA (nfaLastThreeB,"abbb");;
  langNFA (nfaLastThreeB,7);;
 **)

(* 
* A sample DFA for testing
*
* It accepts the language of all strings over {a,b} with a
* multiple-of-3 number of a's.
*
*)

let dfaThreeA = { 
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = [ ("start",'a',"one");
            ("one",'a',"two");
            ("two",'a',"start");
            ("start",'b',"start");
            ("one",'b',"one");
            ("two",'b',"two") ];
  start = "start";
  accepting = ["start"]
} 



(* A sample NFA for testing
 *
 * It accepts the language of all strings over {a,b,c} 
 * whose last three symbols are b's.
 *
*)

let nfaLastThreeB = {
  states = [0;2;3;4];
  alphabet = ['a';'b';'c'];
  delta = [ (0,'a',0);
            (0,'b',0);
            (0,'c',0);
            (0,'b',1);
            (1,'b',2);
            (2,'b',3); ];
  start = 0;
  accepting = [3]
} 




(* This function is the base function that langDFA and
 * langNFA use -- it basically loops through all the strings
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is being way too clever to try to not blow the stack 
 * while enumerating all strings up to a given length. Basically.
 * we enumerate all integer, convert them to base K (where K is the
 * size of the alphabet) and then replace every digit base K by the
 * letter of the alphabet at the corresponding index in the alphabet. 
 *
 * The key is that we can enumerate integers super easily
 *
*)

let langFA accept (fa,n) = 

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
          let ts = to_string fa.alphabet i  in
          let bound = expt (List.length fa.alphabet) i in
          let rec loop2 j = 
            if j < bound then (if accept(fa,ts j) 
                               then print_str (ts j)
                               else ();
                               loop2 (j+1))
            else ()  in
            (loop2 0; loop (i+1))
        else ()  in
        loop 0


(* 
* Tester functions that dump the language accepted by a
* finite automaton, either deterministic or not
*
*)

let langDFA x = langFA acceptDFA x
let langNFA x = langFA acceptNFA x

