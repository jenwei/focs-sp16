(* 

HOMEWORK 5

Name: Jennifer Wei

Email: jennifer.wei@students.olin.edu

Remarks, if any:
- Received help from Dennis about TMs since I somehow missed some notes on that (specifically with the step function)
- Received help from Sidd on debugging step and how to approach run
- Received help from Sophia about TMs
- Hopefully tm_g2_a works correctly - I spent a while debugging and had to rewrite it (ended up I accidentally tried testing with {a,b} instead of {c,d} =__=
- Spent forever on Q3 (at least 6 hours) - it works for the test cases on the homework page, but there may be some issues with the carryin case (I essentially had two sets of states, one for if there was a carryin, and one when there isn't) 
- Did not address the case where w1,w2,w3 are of different lengths

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



(* run returns true if m accepts input string w and false if m rejects w and also print 
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
(* LEFT = 0, RIGHT = 1 *)

let tm_q2_a = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"q7";"acc";"rej"];
                input_alphabet = ["c";"d"];
                tape_alphabet = [">";"c";"d";"x";"_"];
                blank = "_";
                left_marker = ">";
                start = "start";
                accept = "acc";
                reject = "rej";
                delta = (fun inp -> match inp with 

                          | ("start", ">") -> ("q1", ">", 1)

                          (*searching for first letter*)
                          | ("q1", "c") -> ("q2", "x", 1)
                          | ("q1", "d") -> ("q5", "x", 1)
                          | ("q1", "_") -> ("acc", "_", 1)
                          | ("q1", "x") -> ("q2", "x", 1)

                          (*if starts with c*)
                          | ("q2", "c") -> ("q2", "c", 1)
                          | ("q2", "d") -> ("q2", "d", 1)
                          | ("q2", "x") -> ("q2", "x", 1)

                          (*@ end of tape go left*)
                          | ("q2", "_") -> ("q3", "_", 0)

                          (*@ end of tape and no more characters to x out*)
                          | ("q3", ">") -> ("acc", ">", 1)

                          (*skip over already x'd out c's*)
                          | ("q3", "x") -> ("q3", "x", 0)

                          (*x out last c*)
                          | ("q3", "c") -> ("q4", "x", 0)

                          (*rewind to beginning of tape*)
                          | ("q4", "c") -> ("q4", "c", 0)
                          | ("q4", "d") -> ("q4", "d", 0)
                          | ("q4", "x") -> ("q1", "x", 1)

                          (*if starts with d*)
                          | ("q5", "c") -> ("q5", "c", 1)
                          | ("q5", "d") -> ("q5", "d", 1)
                          | ("q5", "x") -> ("q5", "x", 1)

                          (*@ end of tape go left*)
                          | ("q5", "_") -> ("q6", "_", 0)

                          (*@ end of tape and no more characters to x out*)
                          | ("q6", ">") -> ("acc", ">", 1)

                          (*skip over already x'd out d's*)
                          | ("q6", "x") -> ("q6", "x", 0)

                          (*x out last d*)
                          | ("q6", "d") -> ("q7", "x", 0)


                          (*rewind to beginning of tape*)
                          | ("q7", "c") -> ("q7", "c", 0)
                          | ("q7", "d") -> ("q7", "d", 0)
                          | ("q7", "x") -> ("q1", "x", 1)

                          (*if state is accept then stay there*)
                          | ("acc", "c") -> ("acc", "c", 1)
                          | ("acc", "d") -> ("acc", "d", 1)
                          | ("acc", ">") -> ("acc", ">", 1)
                          | ("acc", "_") -> ("acc", "_", 1)
                          | ("acc", "x") -> ("acc", "x", 1)

                          (*wildcard*)
                          | (_,c) -> ("rej", c,1)
                        )};;


(* tm_q2_a tests *)
(**
   run tm_q2_a "";;
   run tm_q2_a "c";;
   run tm_q2_a "cc";;
   run tm_q2_a "dd";;
   run tm_q2_a "cddc";;
   run tm_q2_a "cdc";;
   run tm_q2_a "ccdcc";;
   run tm_q2_a "ccdccc";;
   run tm_q2_a "ccdc";;
   run tm_q2_a "dcdc";;
 **)




let tm_q2_b = { states = ["start";"acc";"rej";"q1";"q2";"q3";"q4z";"q4o";"q5z";"q5o";"q6z";"q6o";"q7z";"q7o";"q8zz";"q8zo";"q8oz";"q8oo";"q9zz";"q9zo";"q9oz";"q9oo";"q10zz";"q10zo";"q10oz";"q10oo";"q11zz";"q11zo";"q11oz";"q11oo";"q12zzz";"q12zoo";"q12ozo";"q12ooz";"q1c";"q2c";"q3c";"q4cz";"q4co";"q5cz";"q5co";"q6cz";"q6co";"q7cz";"q7co";"q8czz";"q8czo";"q8coz";"q8coo";"q9czz";"q9czo";"q9coz";"q9coo";"q10czz";"q10czo";"q10coz";"q10coo";"q11czz";"q11czo";"q11coz";"q11coo";"q12czzz";"q12czoo";"q12cozo";"q12cooz"];
                input_alphabet = ["a";"b"];
                tape_alphabet = [">";"a";"b";"x";"_"];
                blank = "_";
                left_marker = ">";
                start = "start";
                accept = "acc";
                reject = "rej";
                delta = (fun inp -> match inp with 

                          | ("start", ">") -> ("q1", ">", 1)

                          (*searching for first b*)
                          | ("q1", "b") -> ("q2", "x", 1)
                          | ("q1", "_") -> ("acc", "_", 1)
                          | ("q1", "x") -> ("q1", "x", 1)

                          (*skip over other b's and x's*)
                          | ("q2", "b") -> ("q2", "b", 1)
                          | ("q2", "x") -> ("q2", "x", 1)


                          (*find three consecutive a's*)
                          | ("q2", "a") -> ("q3", "x", 1)
                          | ("q3", "a") -> ("q4", "x", 1)
                          | ("q4", "a") -> ("q5", "x", 1)

                          (*skip other a's*)
                          | ("q5", "a") -> ("q5", "a", 1)

                          (*@ end of tape*)
                          | ("q5", "_") -> ("q6", "_", 0)


                          (*rewind to beginning of tape*)
                          | ("q6", "a") -> ("q6", "a", 0)
                          | ("q6", "b") -> ("q6", "b", 0)
                          | ("q6", "x") -> ("q6", "x", 0)

                          (*restart process*)
                          | ("q6", ">") -> ("q1", ">", 1)

                          (*if state is accept then stay there*)
                          | ("acc", "a") -> ("acc", "a", 1)
                          | ("acc", "b") -> ("acc", "b", 1)
                          | ("acc", ">") -> ("acc", ">", 1)
                          | ("acc", "_") -> ("acc", "_", 1)
                          | ("acc", "x") -> ("acc", "x", 1)

                          (*wildcard*)
                          | (_,c) -> ("rej", c,1)
                        )};;


(* tm_q2_b tests *)
(**
   run tm_q2_b "";;
   run tm_q2_b "baaa";;
   run tm_q2_b "bbaaaaaa";;
   run tm_q2_b "bbbaaaaaaaaa";;
   run tm_q2_b "b";;
   run tm_q2_b "ba";;
   run tm_q2_b "baa";;
   run tm_q2_b "baaaa";;
   run tm_q2_b "bbaaaaaaa";;
   run tm_q2_b "aaa";;
   run tm_q2_b "abbb";;
 **)



(* QUESTION 3 *)


let binaryAddition = { states = ["start";"acc";"rej";"q1";"q2";"q3";"q4z";"q4o";"q5z";"q5o";"q6o";"q6z";"q7z";"q7o";"q8zz";"q8zo";"q8oz";"q8oo";"q9zz";"q9zo";"q9oz";"q9oo";"q10zz";"q10zo";"q10oz";"q10oo";"q11zz";"q11zo";"q11oz";"q11oo";"q12zzz";"q12zoo";"q12ozo";"q12ooz";"q1c";"q2c";"q3c";"q4cz";"q4co";"q5cz";"q5co";"q6co";"q6cz";"q7cz";"q7co";"q8czz";"q8czo";"q8coz";"q8coo";"q9czz";"q9czo";"q9coz";"q9coo";"q10czz";"q10czo";"q10coz";"q10coo";"q11czz";"q11czo";"q11coz";"q11coo";"q12czzz";"q12czoo";"q12cozo";"q12cooz";"check";"checkc";"revertCheck";"revertCheckc"];
                       input_alphabet = ["0";"1"; "#"];
                       tape_alphabet = ["0";"1";">";"_";"x"; "#"];
                       blank = "_";
                       left_marker = ">";
                       start = "start";
                       accept = "acc";
                       reject = "rej";
                       delta = (fun inp -> match inp with 
                                 | ("start", ">") -> ("q1", ">", 1)

                                 (*NO CARRY IN*)

                                 (*w1 check that first input is non-empty*)
                                 | ("q1", "0") -> ("q2", "0", 1)
                                 | ("q1", "1") -> ("q2", "1", 1)
                                 | ("q1", "x") -> ("q1", "x", 1)

                                 (*skip to pound sign or x*)
                                 | ("q2", "0") -> ("q2", "0", 1)
                                 | ("q2", "1") -> ("q2", "1", 1)
                                 | ("q2", "x") -> ("q3", "x", 0)
                                 | ("q2", "#") -> ("q3", "#", 0)

                                 (*check smallest unchecked bit of w1*)
                                 | ("q3", "0") -> ("q4z", "x", 1)
                                 | ("q3", "1") -> ("q4o", "x", 1)
                                 | ("q3", "x") -> ("q3", "x", 0)

                                 (*ZERO skip to w2*)
                                 | ("q4z", "x") -> ("q4z", "x", 1)
                                 | ("q4z", "#") -> ("q5z", "#", 1)

                                 (*ONE skip to w2*)
                                 | ("q4o", "x") -> ("q4o", "x", 1)
                                 | ("q4o", "#") -> ("q5o", "#", 1)

                                 (*w2 check that first input is non-empty*)
                                 | ("q5z", "0") -> ("q6z", "0", 1)
                                 | ("q5z", "1") -> ("q6z", "1", 1)
                                 | ("q5z", "x") -> ("q5z", "x", 1)

                                 | ("q5o", "0") -> ("q6o", "0", 1)
                                 | ("q5o", "1") -> ("q6o", "1", 1)
                                 | ("q5o", "x") -> ("q5o", "x", 1)

                                 (*w2 skip to pound sign or x *)
                                 | ("q6z", "0") -> ("q6z", "0", 1)
                                 | ("q6z", "1") -> ("q6z", "1", 1)
                                 | ("q6z", "x") -> ("q7z", "x", 0)
                                 | ("q6z", "#") -> ("q7z", "#", 0)

                                 | ("q6o", "0") -> ("q6o", "0", 1)
                                 | ("q6o", "1") -> ("q6o", "1", 1)
                                 | ("q6o", "x") -> ("q7o", "x", 0)
                                 | ("q6o", "#") -> ("q7o", "#", 0)

                                 (*check smallest unchecked bit of w2*)
                                 | ("q7z", "0") -> ("q8zz", "x", 1)
                                 | ("q7z", "1") -> ("q8zo", "x", 1)
                                 | ("q7z", "x") -> ("q7z", "x", 0)

                                 | ("q7o", "0") -> ("q8oz", "x", 1)
                                 | ("q7o", "1") -> ("q8oo", "x", 1)
                                 | ("q7o", "x") -> ("q7o", "x", 0)


                                 (*skip to w3*)
                                 | ("q8zz", "x") -> ("q8zz", "x", 1)
                                 | ("q8zz", "#") -> ("q9zz", "#", 1)
                                 | ("q8zo", "x") -> ("q8zo", "x", 1)
                                 | ("q8zo", "#") -> ("q9zo", "#", 1)
                                 | ("q8oz", "x") -> ("q8oz", "x", 1)
                                 | ("q8oz", "#") -> ("q9oz", "#", 1)
                                 | ("q8oo", "x") -> ("q8oo", "x", 1)
                                 | ("q8oo", "#") -> ("q9oo", "#", 1)

                                 (*w3 check that first input is non-empty*)
                                 | ("q9zz", "0") -> ("q10zz", "0", 1)
                                 | ("q9zz", "1") -> ("q10zz", "1", 1)
                                 | ("q9zz", "x") -> ("acc", "x", 1)

                                 | ("q9zo", "0") -> ("q10zo", "0", 1)
                                 | ("q9zo", "1") -> ("q10zo", "1", 1)
                                 | ("q9zo", "x") -> ("acc", "x", 1)

                                 | ("q9oz", "0") -> ("q10oz", "0", 1)
                                 | ("q9oz", "1") -> ("q10oz", "1", 1)
                                 | ("q9oz", "x") -> ("acc", "x", 1)

                                 | ("q9oo", "0") -> ("q10oo", "0", 1)
                                 | ("q9oo", "1") -> ("q10oo", "1", 1)
                                 | ("q9oo", "x") -> ("acc", "x", 1)

                                 (*w3 skip to end or x*)
                                 | ("q10zz", "0") -> ("q10zz", "0", 1)
                                 | ("q10zz", "1") -> ("q10zz", "1", 1)
                                 | ("q10zz", "x") -> ("q11zz", "x", 0)
                                 | ("q10zz", "_") -> ("q11zz", "_", 0)

                                 | ("q10zo", "0") -> ("q10zo", "0", 1)
                                 | ("q10zo", "1") -> ("q10zo", "1", 1)
                                 | ("q10zo", "x") -> ("q11zo", "x", 0)
                                 | ("q10zo", "_") -> ("q11zo", "_", 0)

                                 | ("q10oz", "0") -> ("q10oz", "0", 1)
                                 | ("q10oz", "1") -> ("q10oz", "1", 1)
                                 | ("q10oz", "x") -> ("q11oz", "x", 0)
                                 | ("q10oz", "_") -> ("q11oz", "_", 0)

                                 | ("q10oo", "0") -> ("q10oo", "0", 1)
                                 | ("q10oo", "1") -> ("q10oo", "1", 1)
                                 | ("q10oo", "x") -> ("q11oo", "x", 0)
                                 | ("q10oo", "_") -> ("q11oo", "_", 0)

                                 (*check smallest unchecked bit of w3 and go back to start*)
                                 | ("q11zz", "0") -> ("q12zzz", "x", 0)
                                 | ("q11zz", "x") -> ("q11zz", "x", 0)

                                 | ("q11zo", "1") -> ("q12zoo", "x", 0)
                                 | ("q11zo", "x") -> ("q11zo", "x", 0)

                                 | ("q11oz", "1") -> ("q12ozo", "x", 0)
                                 | ("q11oz", "x") -> ("q11oz", "x", 0)

                                 | ("q11oo", "0") -> ("q12ooz", "x", 0)
                                 | ("q11oo", "x") -> ("q11oo", "x", 0)

                                 (*go back to start*)
                                 (*no carry over*)
                                 | ("q12zzz", "0") -> ("q12zzz", "0", 0)
                                 | ("q12zzz", "1") -> ("q12zzz", "1", 0)
                                 | ("q12zzz", "x") -> ("q12zzz", "x", 0)
                                 | ("q12zzz", "#") -> ("q12zzz", "#", 0)
                                 | ("q12zzz", ">") -> ("check", ">", 1)

                                 | ("q12zoo", "0") -> ("q12zoo", "0", 0)
                                 | ("q12zoo", "1") -> ("q12zoo", "1", 0)
                                 | ("q12zoo", "x") -> ("q12zoo", "x", 0)
                                 | ("q12zoo", "#") -> ("q12zoo", "#", 0)
                                 | ("q12zoo", ">") -> ("check", ">", 1)

                                 | ("q12ozo", "0") -> ("q12ozo", "0", 0)
                                 | ("q12ozo", "1") -> ("q12ozo", "1", 0)
                                 | ("q12ozo", "x") -> ("q12ozo", "x", 0)
                                 | ("q12ozo", "#") -> ("q12ozo", "#", 0)
                                 | ("q12ozo", ">") -> ("check", ">", 1)

                                 (*carry over*)
                                 | ("q12ooz", "0") -> ("q12ooz", "0", 0)
                                 | ("q12ooz", "1") -> ("q12ooz", "1", 0)
                                 | ("q12ooz", "x") -> ("q12ooz", "x", 0)
                                 | ("q12ooz", "#") -> ("q12ooz", "#", 0)
                                 | ("q12ooz", ">") -> ("q1c", ">", 1)

                                 (*check full string for x's and #'s*)
                                 | ("check", "x") -> ("check", "x", 1)
                                 | ("check", "#") -> ("check", "#", 1)
                                 | ("check", "_") -> ("acc", "_", 1)
                                 | ("check", "0") -> ("revertCheck", "0", 0)
                                 | ("check", "1") -> ("revertCheck", "1", 0)

                                 |("revertCheck", "x") -> ("revertCheck", "x", 0)
                                 |("revertCheck", "#") -> ("revertCheck", "#", 0)
                                 |("revertCheck", "0") -> ("revertCheck", "0", 0)
                                 |("revertCheck", "1") -> ("revertCheck", "1", 0)
                                 |("revertCheck", ">") -> ("q1", ">", 1)

                                 (*CARRY OVER CASE*)
                                 (*w1 check that first input is non-empty*)
                                 | ("q1c", "0") -> ("q2c", "0", 1)
                                 | ("q1c", "1") -> ("q2c", "1", 1)
                                 | ("q1c", "x") -> ("q1c", "x", 1)

                                 (*skip to pound sign or x*)
                                 | ("q2c", "0") -> ("q2c", "0", 1)
                                 | ("q2c", "1") -> ("q2c", "1", 1)
                                 | ("q2c", "x") -> ("q3c", "x", 0)
                                 | ("q2c", "#") -> ("q3c", "#", 0)

                                 (*check smallest unchecked bit of w1*)
                                 | ("q3c", "0") -> ("q4cz", "x", 1)
                                 | ("q3c", "1") -> ("q4co", "x", 1)
                                 | ("q3c", "x") -> ("q3c", "x", 0)

                                 (*ZERO skip to w2*)
                                 | ("q4cz", "x") -> ("q4cz", "x", 1)
                                 | ("q4cz", "#") -> ("q5cz", "#", 1)

                                 (*ONE skip to w2*)
                                 | ("q4co", "x") -> ("q4co", "x", 1)
                                 | ("q4co", "#") -> ("q5co", "#", 1)

                                 (*w2 check that first input is non-empty*)
                                 | ("q5cz", "0") -> ("q6cz", "0", 1)
                                 | ("q5cz", "1") -> ("q6cz", "1", 1)
                                 | ("q5cz", "x") -> ("q5cz", "x", 1)

                                 | ("q5co", "0") -> ("q6co", "0", 1)
                                 | ("q5co", "1") -> ("q6co", "1", 1)
                                 | ("q5co", "x") -> ("q5co", "x", 1)

                                 (*w2 skip to pound sign or x *)
                                 | ("q6cz", "0") -> ("q6cz", "0", 1)
                                 | ("q6cz", "1") -> ("q6cz", "1", 1)
                                 | ("q6cz", "x") -> ("q7cz", "x", 0)
                                 | ("q6cz", "#") -> ("q7cz", "#", 0)

                                 | ("q6co", "0") -> ("q6co", "0", 1)
                                 | ("q6co", "1") -> ("q6co", "1", 1)
                                 | ("q6co", "x") -> ("q7co", "x", 0)
                                 | ("q6co", "#") -> ("q7co", "#", 0)

                                 (*check smallest unchecked bit of w2*)
                                 | ("q7cz", "0") -> ("q8czz", "x", 1)
                                 | ("q7cz", "1") -> ("q8czo", "x", 1)
                                 | ("q7cz", "x") -> ("q7cz", "x", 0)

                                 | ("q7co", "0") -> ("q8coz", "x", 1)
                                 | ("q7co", "1") -> ("q8coo", "x", 1)
                                 | ("q7co", "x") -> ("q7co", "x", 0)


                                 (*skip to w3*)
                                 | ("q8czz", "x") -> ("q8czz", "x", 1)
                                 | ("q8czz", "#") -> ("q9czz", "#", 1)
                                 | ("q8czo", "x") -> ("q8czo", "x", 1)
                                 | ("q8czo", "#") -> ("q9czo", "#", 1)
                                 | ("q8coz", "x") -> ("q8coz", "x", 1)
                                 | ("q8coz", "#") -> ("q9coz", "#", 1)
                                 | ("q8coo", "x") -> ("q8coo", "x", 1)
                                 | ("q8coo", "#") -> ("q9coo", "#", 1)

                                 (*w3 check that first input is non-empty*)
                                 | ("q9czz", "0") -> ("q10czz", "0", 1)
                                 | ("q9czz", "1") -> ("q10czz", "1", 1)
                                 | ("q9czz", "x") -> ("acc", "x", 1)

                                 | ("q9czo", "0") -> ("q10czo", "0", 1)
                                 | ("q9czo", "1") -> ("q10czo", "1", 1)
                                 | ("q9czo", "x") -> ("acc", "x", 1)

                                 | ("q9coz", "0") -> ("q10coz", "0", 1)
                                 | ("q9coz", "1") -> ("q10coz", "1", 1)
                                 | ("q9coz", "x") -> ("acc", "x", 1)

                                 | ("q9coo", "0") -> ("q10coo", "0", 1)
                                 | ("q9coo", "1") -> ("q10coo", "1", 1)
                                 | ("q9coo", "x") -> ("acc", "x", 1)

                                 (*w3 skip to end or x*)
                                 | ("q10czz", "0") -> ("q10czz", "0", 1)
                                 | ("q10czz", "1") -> ("q10czz", "1", 1)
                                 | ("q10czz", "x") -> ("q11czz", "x", 0)
                                 | ("q10czz", "_") -> ("q11czz", "_", 0)

                                 | ("q10czo", "0") -> ("q10czo", "0", 1)
                                 | ("q10czo", "1") -> ("q10czo", "1", 1)
                                 | ("q10czo", "x") -> ("q11czo", "x", 0)
                                 | ("q10czo", "_") -> ("q11czo", "_", 0)

                                 | ("q10coz", "0") -> ("q10coz", "0", 1)
                                 | ("q10coz", "1") -> ("q10coz", "1", 1)
                                 | ("q10coz", "x") -> ("q11coz", "x", 0)
                                 | ("q10coz", "_") -> ("q11coz", "_", 0)

                                 | ("q10coo", "0") -> ("q10coo", "0", 1)
                                 | ("q10coo", "1") -> ("q10coo", "1", 1)
                                 | ("q10coo", "x") -> ("q11coo", "x", 0)
                                 | ("q10coo", "_") -> ("q11coo", "_", 0)

                                 (*check smallest unchecked bit of w3 and go back to start*)
                                 | ("q11czz", "1") -> ("q12czzz", "x", 0)
                                 | ("q11czz", "x") -> ("q11czz", "x", 0)

                                 | ("q11czo", "0") -> ("q12czoo", "x", 0)
                                 | ("q11czo", "x") -> ("q11czo", "x", 0)

                                 | ("q11coz", "0") -> ("q12cozo", "x", 0)
                                 | ("q11coz", "x") -> ("q11coz", "x", 0)

                                 | ("q11coo", "1") -> ("q12cooz", "x", 0)
                                 | ("q11coo", "x") -> ("q11coo", "x", 0)

                                 (*go back to start*)
                                 (*no carry over*)
                                 | ("q12czzz", "0") -> ("q12czzz", "0", 0)
                                 | ("q12czzz", "1") -> ("q12czzz", "1", 0)
                                 | ("q12czzz", "x") -> ("q12czzz", "x", 0)
                                 | ("q12czzz", "#") -> ("q12czzz", "#", 0)
                                 | ("q12czzz", ">") -> ("check", ">", 1)

                                 (*carry over*)
                                 | ("q12czoo", "0") -> ("q12czoo", "0", 0)
                                 | ("q12czoo", "1") -> ("q12czoo", "1", 0)
                                 | ("q12czoo", "x") -> ("q12czoo", "x", 0)
                                 | ("q12czoo", "#") -> ("q12czoo", "#", 0)
                                 | ("q12czoo", ">") -> ("checkc", ">", 1)

                                 | ("q12cozo", "0") -> ("q12cozo", "0", 0)
                                 | ("q12cozo", "1") -> ("q12cozo", "1", 0)
                                 | ("q12cozo", "x") -> ("q12cozo", "x", 0)
                                 | ("q12cozo", "#") -> ("q12cozo", "#", 0)
                                 | ("q12cozo", ">") -> ("checkc", ">", 1)


                                 | ("q12cooz", "0") -> ("q12cooz", "0", 0)
                                 | ("q12cooz", "1") -> ("q12cooz", "1", 0)
                                 | ("q12cooz", "x") -> ("q12cooz", "x", 0)
                                 | ("q12cooz", "#") -> ("q12cooz", "#", 0)
                                 | ("q12cooz", ">") -> ("checkc", ">", 1)

                                 (*check full string for x's and #'s*)
                                 | ("checkc", "x") -> ("checkc", "x", 1)
                                 | ("checkc", "#") -> ("checkc", "#", 1)
                                 | ("checkc", "_") -> ("acc", "_", 1)
                                 | ("checkc", "0") -> ("revertCheckc", "0", 0)
                                 | ("checkc", "1") -> ("revertCheckc", "1", 0)

                                 |("revertCheckc", "x") -> ("revertCheckc", "x", 0)
                                 |("revertCheckc", "#") -> ("revertCheckc", "#", 0)
                                 |("revertCheckc", "0") -> ("revertCheckc", "0", 0)
                                 |("revertCheckc", "1") -> ("revertCheckc", "1", 0)
                                 |("revertCheckc", ">") -> ("q1c", ">", 1)


                                 (*if state is accept then stay there*)
                                 | ("acc", "0") -> ("acc", "0", 1)
                                 | ("acc", "1") -> ("acc", "1", 1)
                                 | ("acc", ">") -> ("acc", ">", 1)
                                 | ("acc", "_") -> ("acc", "_", 1)
                                 | ("acc", "x") -> ("acc", "x", 1)
                                 | ("acc", "#") -> ("acc", "#", 1)

                                 (*wildcard*)
                                 | (_,c) -> ("rej", c,1)
                               )};;


(* binary addition tests *)
(**
   (*Seem to work*)
   (*false*)
   run binaryAddition "";;
   run binaryAddition "##";;
   run binaryAddition "000";;
   run binaryAddition "000#000";;
   run binaryAddition "000##000";;
   (*true*)
   run binaryAddition "000#000#000";;
   run binaryAddition "001#000#001";;
   (*false*)
   run binaryAddition "000#001#000";; 
   (*true*)
   run binaryAddition "000#001#001";;
   (*false*)
   run binaryAddition "001#000#011";;
   (*true*)
   run binaryAddition "001#001#010";;
   (*false*)
   run binaryAddition "001#001#011";;
   run binaryAddition "001#001#110";;
   (*true*)
   run binaryAddition "101#001#110";;
   run binaryAddition "101#010#111";;
   (*false*)
   run binaryAddition "00111#10101#11110";;
   (*true*)
   run binaryAddition "00111#10101#11100";;
 **)
(**
   (*pseudo-random test case I wrote*)
   run binaryAddition "111#111#10000";;
 **)

