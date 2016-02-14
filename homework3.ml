(* 

HOMEWORK 3

Name: Jennifer Wei

Email: jennifer.wei@students.olin.edu

Remarks, if any:
- My compiler was super unhappy with the use of String.create and String.set in implode
so I had to switch it to Bytes - weird
- I don't fully understand how steps works - I knew the general structure that/format it needed to be in and tested until the test cases worked (will follow up with others to resolve my confusion)
- Got help from Dennis with how to approach isDFA
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


let dfa_q2_a = { states = [0];
                 		 alphabet = ['a'];
                 		 delta = [ ];
                 		 start = 0;
                 		 accepting = []}


let dfa_q2_b = { states = [0];
                 		 alphabet = ['a'];
                 		 delta = [ ];
                 		 start = 0;
                 		 accepting = []}


let dfa_q2_c = { states = [0];
                 		 alphabet = ['a'];
                 		 delta = [ ];
                 		 start = 0;
                 		 accepting = []}


let nfa_q2_d = { states = [0];
                 		 alphabet = ['a'];
                 		 delta = [ ];
                 		 start = 0;
                 		 accepting = []}




(* QUESTION 3 *)


let keepTarget (trs) = failwith "keepTarget not implemented"
;;

(* keepTarget tests *)


let isAcceptingAny (fa,qs) = failwith "isAcceptingAny not implemented"
;;

(* isAcceptingAny tests *)


let rec stepAll (fa,qs,a) = failwith "stepAll not implemented"
;;

(* stepAll tests *) 


let rec stepsAll (fa,qs,syms) = failwith "stepsAll not implemented"
;;

(* stepsAll tests *)


let acceptNFA (fa,input) = failwith "acceptNFA not implemented"
;;

(* acceptNFA tests *)



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

