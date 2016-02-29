(* 

HOMEWORK 5

Name: Jennifer Wei

Email: jennifer.wei@students.olin.edu

Remarks, if any:
- Got help from Dennis about TMs since I somehow missed some notes on that (specifically with the step function)
- Received help from Sidd on debugging step

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
* String <-> characters utility functions:
*
*   explode : string -> string list
*      returns the list of characters making up a string
*
*)

let explode str = 
  let rec acc index result = 
    if (index<0) then result
    else acc (index-1) ((String.sub str index 1)::result) in
    acc (String.length(str)-1) []


(*
* Type for deterministic Turing machines
*
* Parameterized by type for states
*)

type symbol = string

type 'a tm = { states : 'a list;
               input_alphabet : symbol list;
               tape_alphabet : symbol list;
               left_marker : symbol;
               blank : symbol;
               delta : ('a * symbol) -> ('a * symbol * int);   (* 0 = Left, 1 = Right *)
               start : 'a;
               accept : 'a;
               reject : 'a }

type 'a config = { state : 'a;
                   before: symbol list;
                   after: symbol list }

(*
* Helper function
*
* Pint a configuration (including newline) to standard output
* and RETURN A VALUE
* 
*)

let printConfig m config value = 
  let mw = List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
  let _ = print_string (String.sub (config.state^(String.make mw ' ')) 0 mw) in
  let print_syms = List.iter (Printf.printf " %s ")  in
  let _ = print_string "  "  in
  let _ = print_syms config.before  in
  let _ = (match config.after with 
            | [] -> Printf.printf "[%s]" m.blank
            | a::v' -> let _ = Printf.printf "[%s]" a  in
                  print_syms v') in
  let _ = print_newline ()  in
    value




(* QUESTION 1 *)


let startConfig m w = {
  state = m.start;
  before = [];
  after = m.left_marker :: explode w
}
;;

(* startConfig tests *)
(**
   startConfig asbs "";;
   startConfig asbs "ab";;
   startConfig asbs "aaaabbbaa";;
   startConfig anbn "";;
   startConfig anbn "aabb";;
   startConfig anbn "aabbaa";;
 **)


let acceptConfig m config = m.accept = config.state;;

(* acceptConfig tests *)
(**
   acceptConfig asbs {state="start"; before=[]; after=["_"]};;
   acceptConfig asbs {state="q1"; before=[]; after=[">";"a";"_"]};;
   acceptConfig asbs {state="acc"; before=["b"]; after=[">";"a";"_"]};;
   acceptConfig asbs {state="rej"; before=["b"]; after=[">";"a";"_"]};;
 **)


let rejectConfig m config = m.reject = config.state;;

(* rejectConfig tests *)
(**
   rejectConfig asbs {state="start"; before=[]; after=["_"]};;
   rejectConfig asbs {state="q1"; before=[]; after=[">";"a";"_"]};;
   rejectConfig asbs {state="acc"; before=["b"]; after=[">";"a";"_"]};;
   rejectConfig asbs {state="rej"; before=["b"]; after=[">";"a";"_"]};;
 **)


(* halt if in accept state or reject state *)
let haltConfig m c = (acceptConfig m c) || (rejectConfig m c);;

(* haltConfig tests *)
(**
   haltConfig asbs {state="start"; before=[]; after=["_"]};;
   haltConfig asbs {state="q1"; before=[]; after=[">";"a";"_"]};;
   haltConfig asbs {state="acc"; before=["b"]; after=[">";"a";"_"]};;
   haltConfig asbs {state="rej"; before=["b"]; after=[">";"a";"_"]};;
 **)

(* SAMPLE TM FOR EASY TESTING *)
let asbs = { states = ["start"; "q1"; "acc"; "rej"];
             input_alphabet = ["a";"b"];
             tape_alphabet = ["a";"b";"_";">"];
             blank = "_";
             left_marker = ">";
             start = "start";
             accept = "acc";
             reject = "rej";
             delta = (fun inp -> match inp with
                       | ("start", "a") -> ("start", "a", 1)
                       | ("start", "b") -> ("q1", "b", 1)
                       | ("start", ">") -> ("start", ">", 1)
                       | ("start", "_") -> ("acc", "_", 1)
                       | ("q1", "b") -> ("q1", "b", 1)
                       | ("q1", "_") -> ("acc", "_", 1)
                       | ("acc", "a") -> ("acc", "a", 1)
                       | ("acc", "b") -> ("acc", "b", 1)
                       | ("acc", ">") -> ("acc", ">", 1)
                       | ("acc", "_") -> ("acc", "_", 1)
                       | (_,c) -> ("rej",c,1))
           }
;;

let anbn = { states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
             input_alphabet = ["a";"b"];
             tape_alphabet = ["a";"b";"X";"/";"|"];
             blank = "/";
             left_marker = "|";
             start = "start";
             accept = "acc";
             reject = "rej";
             delta = (fun inp -> match inp with
                       | ("start", "a") -> ("start", "a", 1)
                       | ("start", "b") -> ("q1", "b", 1)
                       | ("start", "|") -> ("start", "|", 1)
                       | ("start", "/") -> ("q2", "/", 1)
                       | ("q1", "b") -> ("q1", "b", 1)
                       | ("q1", "/") -> ("q2", "/", 1)
                       | ("q2", "|") -> ("q3", "|", 1)
                       | ("q2", "a") -> ("q2", "a", 0)
                       | ("q2", "b") -> ("q2", "b", 0)
                       | ("q2", "X") -> ("q2", "X", 0)
                       | ("q2", "/") -> ("q2", "/", 0)
                       | ("q3", "X") -> ("q3", "X", 1)
                       | ("q3", "/") -> ("acc", "/", 1)
                       | ("q3", "a") -> ("q4", "X", 1)
                       | ("q4", "a") -> ("q4", "a", 1)
                       | ("q4", "X") -> ("q4", "X", 1)
                       | ("q4", "b") -> ("q2", "X", 1)
                       | ("acc", "a") -> ("acc", "a", 1)
                       | ("acc", "b") -> ("acc", "b", 1)
                       | ("acc", "|") -> ("acc", "|", 1)
                       | ("acc", "X") -> ("acc", "X", 1)
                       | ("acc", "/") -> ("acc", "/", 1)
                       | (_,c) -> ("rej",c,1))}
;;

let anbncn = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
               input_alphabet = ["a";"b";"c"];
               tape_alphabet = ["a";"b";"c";"X";"_";">"];
               blank = "_";
               left_marker = ">";
               start = "start";
               accept = "acc";
               reject = "rej";
               delta = (fun inp -> match inp with
                         | ("start", "a") -> ("start", "a", 1)
                         | ("start", "b") -> ("q1", "b", 1)
                         | ("start", "c") -> ("q6", "c", 1)
                         | ("start", ">") -> ("start", ">", 1)
                         | ("start", "_") -> ("q2", "_", 1)
                         | ("q1", "b") -> ("q1", "b", 1)
                         | ("q1", "c") -> ("q6", "c", 1)
                         | ("q1", "_") -> ("q2", "_", 1)
                         | ("q2", ">") -> ("q3", ">", 1)
                         | ("q2", "a") -> ("q2", "a", 0)
                         | ("q2", "b") -> ("q2", "b", 0)
                         | ("q2", "c") -> ("q2", "c", 0)
                         | ("q2", "_") -> ("q2", "_", 0)
                         | ("q2", "X") -> ("q2", "X", 0)
                         | ("q3", "X") -> ("q3", "X", 1)
                         | ("q3", "_") -> ("acc", "_", 1)
                         | ("q3", "a") -> ("q4", "X", 1)
                         | ("q4", "a") -> ("q4", "a", 1)
                         | ("q4", "X") -> ("q4", "X", 1)
                         | ("q4", "b") -> ("q5", "X", 1)
                         | ("q5", "b") -> ("q5", "b", 1)
                         | ("q5", "X") -> ("q5", "X", 1)
                         | ("q5", "c") -> ("q2", "X", 1)
                         | ("q6", "c") -> ("q6", "c", 1)
                         | ("q6", "_") -> ("q2", "_", 1)
                         | ("acc", "a") -> ("acc", "a", 1)
                         | ("acc", "b") -> ("acc", "b", 1)
                         | ("acc", "c") -> ("acc", "c", 1)
                         | ("acc", ">") -> ("acc", ">", 1)
                         | ("acc", "X") -> ("acc", "X", 1)
                         | ("acc", "_") -> ("acc", "_", 1)
                         | (_,c) -> ("rej", c,1))}
;;


(* step Helper to find tail *)
let stepHelperAfter m l = 
  match l with
    | [] -> [m.blank]
    | [h] -> [m.blank]
    | h::t -> t
;;


let step m config = 
  (* q is the new state, b is before, and dir is the direction (0 is left, 1 is right) *)
  let (q,b,dir) = m.delta (config.state,List.hd(config.after)) in

  let cb = config.before in 
    if (dir = 0) (* left *) then
      (* reverse the list and get the head to get the last state of before *)
      let prev = List.hd (List.rev(config.before)) in
      let tail = List.tl (config.after) in
      let newb = List.rev (List.tl (List.rev(config.before))) in
        {
          state = q;
          before = newb;
          after = prev::b::tail;
        }
    else 
      { (* right *)
        state = q;
        before = cb @ [b] (*take the old before and append [b] to it*);
        after = stepHelperAfter m config.after
      };

;;


(* step tests *)
(**
   step asbs {state="start"; before=[]; after=["_"]};;
   step asbs {state="start"; before=[">";"a"]; after=["b";"b"]};;
   step asbs {state="q1"; before=[">";"a"]; after=["a";"b"]};;
   step asbs {state="q1"; before=[">";"a"]; after=["b";"b"]};;

   step anbn {state="q1"; before=["|";"a";"b"]; after=["/"]};;
   step anbn {state="q2"; before=["|";"a";"b"]; after=["/"]};;
   step anbn {state="q3"; before=["|"]; after=["a";"b"]};;
   step anbn {state="q4"; before=["|";"X"]; after=["b"]};;
 **)



(* run returns true if m accepts input string w and flase if m rejects w and also print 
   sequence of configs that m goes through during computation *) 
let rec runHelper m config =
  (* check if it's accepting or rejecting and return true/false or check the next config *)
  if acceptConfig m (printConfig m config config) then true
  else if rejectConfig m config then false
  else let nextConfig = step m config in
      runHelper m nextConfig
;;


let run m w = runHelper m (startConfig m w);;


(* run tests *)
(** 
  run asbs "aab";;
  run anbn "aabb";;
  run anbncn "aabbcc";;
  run anbn "aabbbb";;
 **)


  (* 
  * Some sample deterministic Turing machines
  *
  * asbs is the regular language {a^m b^n | m,n >= 0}
  * anbn is the non-regular language {a^n b^n | n >= 0}
  * anbncn is the non-regular language {a^n b^n c^n | n >= 0}
  *
  *)

  let asbs = { states = ["start"; "q1"; "acc"; "rej"];
  input_alphabet = ["a";"b"];
  tape_alphabet = ["a";"b";"_";">"];
  blank = "_";
  left_marker = ">";
  start = "start";
  accept = "acc";
  reject = "rej";
  delta = (fun inp -> match inp with
  | ("start", "a") -> ("start", "a", 1)
  | ("start", "b") -> ("q1", "b", 1)
  | ("start", ">") -> ("start", ">", 1)
  | ("start", "_") -> ("acc", "_", 1)
  | ("q1", "b") -> ("q1", "b", 1)
  | ("q1", "_") -> ("acc", "_", 1)
  | ("acc", "a") -> ("acc", "a", 1)
  | ("acc", "b") -> ("acc", "b", 1)
  | ("acc", ">") -> ("acc", ">", 1)
  | ("acc", "_") -> ("acc", "_", 1)
  | (_,c) -> ("rej",c,1))
  }

  let anbn = { states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
  input_alphabet = ["a";"b"];
  tape_alphabet = ["a";"b";"X";"/";"|"];
  blank = "/";
  left_marker = "|";
  start = "start";
  accept = "acc";
  reject = "rej";
  delta = (fun inp -> match inp with
  | ("start", "a") -> ("start", "a", 1)
  | ("start", "b") -> ("q1", "b", 1)
  | ("start", "|") -> ("start", "|", 1)
  | ("start", "/") -> ("q2", "/", 1)
  | ("q1", "b") -> ("q1", "b", 1)
  | ("q1", "/") -> ("q2", "/", 1)
  | ("q2", "|") -> ("q3", "|", 1)
  | ("q2", "a") -> ("q2", "a", 0)
  | ("q2", "b") -> ("q2", "b", 0)
  | ("q2", "X") -> ("q2", "X", 0)
  | ("q2", "/") -> ("q2", "/", 0)
  | ("q3", "X") -> ("q3", "X", 1)
  | ("q3", "/") -> ("acc", "/", 1)
  | ("q3", "a") -> ("q4", "X", 1)
  | ("q4", "a") -> ("q4", "a", 1)
  | ("q4", "X") -> ("q4", "X", 1)
  | ("q4", "b") -> ("q2", "X", 1)
  | ("acc", "a") -> ("acc", "a", 1)
  | ("acc", "b") -> ("acc", "b", 1)
  | ("acc", "|") -> ("acc", "|", 1)
  | ("acc", "X") -> ("acc", "X", 1)
  | ("acc", "/") -> ("acc", "/", 1)
  | (_,c) -> ("rej",c,1))}


  let anbncn = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
  input_alphabet = ["a";"b";"c"];
  tape_alphabet = ["a";"b";"c";"X";"_";">"];
  blank = "_";
  left_marker = ">";
  start = "start";
  accept = "acc";
  reject = "rej";
  delta = (fun inp -> match inp with
  | ("start", "a") -> ("start", "a", 1)
  | ("start", "b") -> ("q1", "b", 1)
  | ("start", "c") -> ("q6", "c", 1)
  | ("start", ">") -> ("start", ">", 1)
  | ("start", "_") -> ("q2", "_", 1)
  | ("q1", "b") -> ("q1", "b", 1)
  | ("q1", "c") -> ("q6", "c", 1)
  | ("q1", "_") -> ("q2", "_", 1)
  | ("q2", ">") -> ("q3", ">", 1)
  | ("q2", "a") -> ("q2", "a", 0)
  | ("q2", "b") -> ("q2", "b", 0)
  | ("q2", "c") -> ("q2", "c", 0)
  | ("q2", "_") -> ("q2", "_", 0)
  | ("q2", "X") -> ("q2", "X", 0)
  | ("q3", "X") -> ("q3", "X", 1)
  | ("q3", "_") -> ("acc", "_", 1)
  | ("q3", "a") -> ("q4", "X", 1)
  | ("q4", "a") -> ("q4", "a", 1)
  | ("q4", "X") -> ("q4", "X", 1)
  | ("q4", "b") -> ("q5", "X", 1)
  | ("q5", "b") -> ("q5", "b", 1)
  | ("q5", "X") -> ("q5", "X", 1)
  | ("q5", "c") -> ("q2", "X", 1)
  | ("q6", "c") -> ("q6", "c", 1)
  | ("q6", "_") -> ("q2", "_", 1)
  | ("acc", "a") -> ("acc", "a", 1)
  | ("acc", "b") -> ("acc", "b", 1)
  | ("acc", "c") -> ("acc", "c", 1)
  | ("acc", ">") -> ("acc", ">", 1)
  | ("acc", "X") -> ("acc", "X", 1)
  | ("acc", "_") -> ("acc", "_", 1)
  | (_,c) -> ("rej", c,1))}



  (* QUESTION 2 *)

  (* THESE ARE PLACEHOLDERS - THEY DEFINE EMPTY TURING MACHINES *)
  (* REPLACE BY YOUR OWN DEFINITIONS *)


  let tm_q2_a = { states = ["x"];
  input_alphabet = ["x"];
  tape_alphabet = ["x"];
  blank = "x";
  left_marker = "x";
  start = "x";
  accept = "x";
  reject = "x";
  delta = (fun (x,y) -> (x,y,0))}


  let tm_q2_b = { states = ["x"];
  input_alphabet = ["x"];
  tape_alphabet = ["x"];
  blank = "x";
  left_marker = "x";
  start = "x";
  accept = "x";
  reject = "x";
  delta = (fun (x,y) -> (x,y,0))}




  (* QUESTION 3 *)


  let binaryAddition = { states = ["x"];
  input_alphabet = ["x"];
  tape_alphabet = ["x"];
  blank = "x";
  left_marker = "x";
  start = "x";
  accept = "x";
  reject = "x";
  delta = (fun (x,y) -> (x,y,0))}

