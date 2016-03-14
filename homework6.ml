(* 

HOMEWORK 6

Name: Jennifer Wei

Email: jennifer.wei@students.olin.edu

Remarks, if any:
- Got help from Kyle, Sophie, and Dennis about how transformDelta works (it was difficult to wrap my mind around the whole transformDelta returns a function for some reason) but Sophie helped clarify by showing me how to explicitly show that using 'let transformDelta states delta f = fun (a,b) ->'
- Got help from Kyle with Q3 - still a bit confused about structured states - and spent a while working on the last problem mainly because I wasn't sure how to represent structured states in the TM (and also because I think my brain was ready to go on spring break)
- Spent 2+ hours debugging 'permutations' - works for the test cases provided, but I made the TM more complicated than it needed to be, so I could imagine there being some bug/edge case not accounted for, so sorry in advance about that.
- For 'copies' - shouldn't it not accept ε since it's not in {0,1}?
- Received help with 'copies' - switched the value checkers (the second variable of the tuple) to simplify things (spent an embarrassing amount of time debugging - a lot of syntax errors)
- The interface I've been using (ocaml-top) has the unfortunate feature where a warning in the console prevents the results from being printed, so unfortunately debugging took a lot longer as I had to keep copy and pasting code between ocaml-top and an online compiler (codepad) :(

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
*
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
*   Code to run a string tm machine
*
*)

let run m w = 

let printConfig m config value = 
let mw = 
List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
let _ = 
print_string (String.sub (config.state^(String.make mw ' ')) 0 mw) in
let print_syms = List.iter (Printf.printf " %s ")  in
let _ = print_string "  "  in
let _ = print_syms config.before  in
let _ = (match config.after with 
| [] -> Printf.printf "[%s]" m.blank
| a::v' -> let _ = Printf.printf "[%s]" a  in
print_syms v') in
let _ = print_newline ()  in
value  in

let acceptConfig m config = (config.state=m.accept) in

let rejectConfig m config = (config.state=m.reject) in

let haltConfig m c = (acceptConfig m c) || (rejectConfig m c) in

let startConfig m w = 
{ state=m.start;before = [];after = m.left_marker::(explode w)} in

let rec last u = 
match u with
| [] -> failwith "Moving Left from leftmost tape position"
| [a] -> ([],a)
| x::xs -> let (u',r) = last xs  in (x::u',r)   in

let step m config = 
if (haltConfig m config) then config
else let (a,v') = match config.after with
| [] -> (m.blank,[])
| a::v' -> (a,v')  in
let (q',b,dir) = m.delta(config.state,a) in
if dir = 0  (* left *)
then let (u',c) = last config.before in 
{state=q';before=u';after=c::b::v'}
else {state=q';before=config.before@[b];after=v'} in

let rec loop c = 
let _ = printConfig m c c in
if  (acceptConfig m c) then true
else if (rejectConfig m c) then false
else loop (step m c)  in

loop (startConfig m w)


let rec pairs xs ys =
List.fold_right (fun x r -> (List.map (fun y -> (x,y)) ys)@r) xs []



(* QUESTION 1 *)

(* returns the list of all triples (a,b,c) where a is an element of xs, b is an element of ys, and c is an element of zs*) 
let triples xs ys zs = 
(* took code from pairs and modified it to work for triples *)
List.fold_right (fun a rest -> 
(List.map (fun (b,c) -> (a,b,c)) 
(pairs ys zs)
)
@rest) xs []
;;

(* triples tests *)
(**
triples [] [] [];;
triples [] ["a";"b"] [100;101];;
triples [1;2] [] [100;101];;
triples [1;2] ["a";"b"] [];;
triples [1] ["a"] [100];;
triples [1;2;3] ["a";"b"] [100;101;102];;
**)



(* returns the list of all tuples (a,b,c,d) where a is an element of xs, b is an element of ys, c is an element of zs and d is an element of ws *)
let quads xs ys zs ws = 
(* modify code from triples *)
List.fold_right (fun a rest -> 
(List.map (fun (b,c,d) -> (a,b,c,d)) 
(triples ys zs ws)
)
@rest) xs []
;;

(* quads tests *)
(**
quads [] [] [] [];;
quads [] ["a";"b"] [100;101] [true;false];;
quads [1;2] [] [100;101] [true;false];;
quads [1;2] ["a";"b"] [] [true;false];;
quads [1;2] ["a";"b"] [100;101] [];;
quads [1] ["a"] [100] [true];;
quads [1;2] ["a";"b"] [100;101] [true;false];;
**)



let rec range n = 
if n<0 then [] else n::(range (n-1))
;;

(* range tests *)
(**
range (-1);;
range 0;;
range 3;;
range 17;;
**)



(* QUESTION 2 *)
let trans (x,y) = x^"/"^(string_of_int y);;

(* returns the result of applying transformation f to every state in states *)
let transformStates states f = 
List.map f states;;

(* transformStates tests *)
(**
transformStates [] trans;;
transformStates [("a",1);("b",2);("c",3)] trans;;
**)



(* returns the state s in states for which (f s) is a target *)
let rec find_original states f target = 
match states with 
| [] -> failwith "cannot find original value"
(* ts is a single transformed state *)
| s::rest -> let ts = f (s) in 
if ts = target then s else find_original rest f target
;;

(* find_original tests *)
(** 
find_original [("a",1);("b",2);("c",3)] trans "a/1";; (* ("a", 1) *)
find_original [("a",1);("b",2);("c",3)] trans "b/2";;
find_original [("a",1);("b",2);("c",3)] trans "c/3";;
find_original [("a",1);("b",2);("c",3)] trans "d/3";;
**) 



let transformDelta states delta f = fun (a,b) -> 
(* find original form of input *)
let oform = find_original states f a in
(* run input through original delta *)
let dout = delta (oform, b) in
(* apply transformation f before outputting it *)
match dout with 
| (q,b,d) -> ((f q),b,d)
;;

(* transformDelta test *)
(** 
let trans (x,y) = x^"/"^(string_of_int y);;

let delta x = match x with
| (("a",1),"0") -> (("b",2),"0",0)
| (("a",1),"1") -> (("c",3),"1",0)
| (("b",2),"0") -> (("c",3),"0",0)
| (("b",2),"1") -> (("a",1),"1",0)
| (("c",3),"0") -> (("a",1),"0",1)
| (("c",3),"1") -> (("b",2),"1",1)
| (_,sym) -> (("a",1),sym,1)  ;;

delta(("a",1),"0");;
delta(("b",2),"0");;
delta(("c",3),"0");;

let new_delta = transformDelta [("a",1);("b",2);("c",3)] delta trans;;

new_delta("a/1","0");; (*("b/2", "0", 0)*)
**)



(* cut and pasted template from above and commented changes! *) 
let transform m f = {
states = transformStates m.states f; (* transform the state *)
input_alphabet = m.input_alphabet;
tape_alphabet = m.tape_alphabet; 
left_marker = m.left_marker;
blank = m.blank; 
delta = transformDelta m.states m.delta f; (* transform the delta *)
start = f m.start;  (* apply function to var *)
accept = f m.accept; (* apply function to var *)
reject = f m.reject (* apply function to var *)
};;




(* 
* Some sample deterministic Turing machines with structured states
*
* anbn is the non-regular language {a^n b^n | n >= 0}
* add1  accepts strings u#v where v = u+1 in binary
*
*)


let anbn = { states = [ ("start",0); 
("q",1);
("q",2);
("q",3);
("q",4);
("acc",0);
("rej",0) ];
input_alphabet = ["a";"b"];
tape_alphabet = ["a";"b";"X";"/";"|"];
blank = "/";
left_marker = "|";
start = ("start",0);
accept = ("acc",0);
reject = ("rej",0);
delta = (fun inp -> match inp with
| (("start",0), "a") -> (("start",0), "a", 1)
| (("start",0), "b") -> (("q",1), "b", 1)
| (("start",0), "|") -> (("start",0), "|", 1)
| (("start",0), "/") -> (("q",2), "/", 1)
| (("q",1), "b") -> (("q",1), "b", 1)
| (("q",1), "/") -> (("q",2), "/", 1)
| (("q",2), "|") -> (("q",3), "|", 1)
| (("q",2), "a") -> (("q",2), "a", 0)
| (("q",2), "b") -> (("q",2), "b", 0)
| (("q",2), "X") -> (("q",2), "X", 0)
| (("q",2), "/") -> (("q",2), "/", 0)
| (("q",3), "X") -> (("q",3), "X", 1)
| (("q",3), "/") -> (("acc",0), "/", 1)
| (("q",3), "a") -> (("q",4), "X", 1)
| (("q",4), "a") -> (("q",4), "a", 1)
| (("q",4), "X") -> (("q",4), "X", 1)
| (("q",4), "b") -> (("q",2), "X", 1)
| (("acc",0), s) -> (("acc",0),s,1)
| (_,c) -> (("rej",0),c,1))}


let add1 = 
{ states =    (* spelled out fully so as not to rely on 'triples' *)
[("start", -1, -1); ("start", -1, 0); ("start", -1, 1); ("start", 0, -1);
("start", 0, 0); ("start", 0, 1); ("start", 1, -1); ("start", 1, 0);
("start", 1, 1); ("check1", -1, -1); ("check1", -1, 0); ("check1", -1, 1);
("check1", 0, -1); ("check1", 0, 0); ("check1", 0, 1); ("check1", 1, -1);
("check1", 1, 0); ("check1", 1, 1); ("check2", -1, -1); ("check2", -1, 0);
("check2", -1, 1); ("check2", 0, -1); ("check2", 0, 0); ("check2", 0, 1);
("check2", 1, -1); ("check2", 1, 0); ("check2", 1, 1); ("rewind", -1, -1);
("rewind", -1, 0); ("rewind", -1, 1); ("rewind", 0, -1); ("rewind", 0, 0);
("rewind", 0, 1); ("rewind", 1, -1); ("rewind", 1, 0); ("rewind", 1, 1);
("go-end-1", -1, -1); ("go-end-1", -1, 0); ("go-end-1", -1, 1);
("go-end-1", 0, -1); ("go-end-1", 0, 0); ("go-end-1", 0, 1);
("go-end-1", 1, -1); ("go-end-1", 1, 0); ("go-end-1", 1, 1);
("go-end-2", -1, -1); ("go-end-2", -1, 0); ("go-end-2", -1, 1);
("go-end-2", 0, -1); ("go-end-2", 0, 0); ("go-end-2", 0, 1);
("go-end-2", 1, -1); ("go-end-2", 1, 0); ("go-end-2", 1, 1);
("skip", -1, -1); ("skip", -1, 0); ("skip", -1, 1); ("skip", 0, -1);
("skip", 0, 0); ("skip", 0, 1); ("skip", 1, -1); ("skip", 1, 0);
("skip", 1, 1); ("scan-1", -1, -1); ("scan-1", -1, 0); ("scan-1", -1, 1);
("scan-1", 0, -1); ("scan-1", 0, 0); ("scan-1", 0, 1); ("scan-1", 1, -1);
("scan-1", 1, 0); ("scan-1", 1, 1); ("scan-2", -1, -1); ("scan-2", -1, 0);
("scan-2", -1, 1); ("scan-2", 0, -1); ("scan-2", 0, 0); ("scan-2", 0, 1);
("scan-2", 1, -1); ("scan-2", 1, 0); ("scan-2", 1, 1);
("check-done", -1, -1); ("check-done", -1, 0); ("check-done", -1, 1);
("check-done", 0, -1); ("check-done", 0, 0); ("check-done", 0, 1);
("check-done", 1, -1); ("check-done", 1, 0); ("check-done", 1, 1)];
input_alphabet = ["0";"1";"#"];
tape_alphabet = ["0";"1";"#";"X";"_";">"];
blank = "_";
left_marker = ">";
start = ("start",-1,-1);
accept = ("acc",-1,-1);
reject = ("rej",-1,-1);
delta = (fun x -> match x with
| (("start",-1,-1),">") -> (("check1",-1,-1),">",1)
| (("check1",-1,-1),"0") -> (("check1",-1,-1),"0",1)
| (("check1",-1,-1),"1") -> (("check1",-1,-1),"1",1)
| (("check1",-1,-1),"#") -> (("check2",-1,-1),"#",1)
| (("check2",-1,-1),"0") -> (("check2",-1,-1),"0",1)
| (("check2",-1,-1),"1") -> (("check2",-1,-1),"1",1)
| (("check2",-1,-1),"_") -> (("rewind",-1,1),"_",0)   (* start with a carry of 1! *)

| (("rewind",-1,carry),">") -> (("go-end-1",-1,carry),">",1)
| (("rewind",-1,carry),"0") -> (("rewind",-1,carry),"0",0)
| (("rewind",-1,carry),"1") -> (("rewind",-1,carry),"1",0)
| (("rewind",-1,carry),"#") -> (("rewind",-1,carry),"#",0)
| (("rewind",-1,carry),"X") -> (("rewind",-1,carry),"X",0)

| (("go-end-1",-1,carry),"#") -> (("scan-1",-1,carry),"#",0)
| (("go-end-1",-1,carry),sym) -> (("go-end-1",-1,carry),sym,1)

| (("scan-1",-1,carry),"X") -> (("scan-1",-1,carry),"X",0)
| (("scan-1",-1,carry),"0") -> (("skip",0,carry),"X",1)
| (("scan-1",-1,carry),"1") -> (("skip",1,carry),"X",1)
| (("scan-1",-1,0),">") -> (("check-done",-1,-1),">",1)  (* carry should be 0 to be done *)

| (("skip",v,carry),"#") -> (("go-end-2",v,carry),"#",1)
| (("skip",v,carry),"X") -> (("skip",v,carry),"X",1)

| (("go-end-2",v,carry),"_") -> (("scan-2",v,carry),"_",0)
| (("go-end-2",v,carry),sym) -> (("go-end-2",v,carry),sym,1)

| (("scan-2",v,carry),"X") -> (("scan-2",v,carry),"X",0)
| (("scan-2",v,carry),"0") when (v+carry) mod 2 = 0 -> (("rewind",-1,(v+carry) / 2),"X",0)
| (("scan-2",v,carry),"1") when (v+carry) mod 2 = 1 -> (("rewind",-1,(v+carry) / 2),"X",0)

| (("check-done",-1,-1),"_") -> (("acc",-1,-1),"_",1)
| (("check-done",-1,-1),"X") -> (("check-done",-1,-1),"X",1)
| (("check-done",-1,-1),"#") -> (("check-done",-1,-1),"#",1)

| (_,sym) -> (("rej",-1,-1),sym,1))}





(* QUESTION 3 *)


(* code from http://stackoverflow.com/questions/10413930/using-fold-left-to-search-for-an-element-in-ocaml *) 
let exists k l = 
List.fold_left (fun b x -> b || x = k) false l
;;

(* a *)
let permutationTM = 
let alphabet = (explode "abcdefghijklmnopqrstuvwxyz") in 
{ states = pairs ["start";"checkU";"checkV";"checkNoU";"checkNoU_V";"rev";"crossU";"skipU";"crossV";"revV";"revU";"revVDone";"revUDone";"acc";"rej";"finalSweep"] ((explode ">_X#")@alphabet);
input_alphabet = "#"::alphabet;
tape_alphabet = (explode ">_X#abcdefghijklmnopqrstuvwxyz");
start = ("start",">");
accept = ("acc","X");
reject = ("rej","X");
blank = "_";
left_marker = ">";
delta = (fun x -> 
match x with
(* scan through string to make sure it has the right shape *)
| (("start",">"),">") -> (("checkNoU",">"),">",1)
| (("checkNoU",">"),"#") -> (("checkNoU_V",">"),"#",1)
| (("checkNoU",">"),letter) -> if (exists letter alphabet) then (("checkU",">"),letter,1) else (("rej","X"),letter,1)
| (("checkU",">"),"#") -> (("checkV",">"),"#",1)
| (("checkU",">"),letter) -> if (exists letter alphabet) then (("checkU",">"),letter,1) else (("rej","X"),letter,0)

| (("checkNoU_V",">"),"_") -> (("acc","X"),"_",0)
| (("checkNoU_V",">"),letter) -> (("rej","X"),"_",0)

| (("checkV",">"),"_") -> (("rev",">"),"_",0)
| (("checkV",">"),letter) -> if (exists letter alphabet) then (("checkV",">"),letter,1) else (("rej","X"),letter,1)

(* reverse to beginning of string *)
| (("rev",sym),">") -> (("crossU",sym),">",1)
| (("rev",sym),letter) -> (("rev",sym),letter,0)

(* cross out first letter, save that to the tuple *)
| (("crossU",sym),"X") -> (("crossU",sym),"X",1)
| (("crossU",sym),"#") -> (("crossV",sym),"#",1)
| (("crossU",sym),letter) -> (("skipU",letter),"X",1)

(* skip to V *)
| (("skipU",sym),"#") -> (("crossV",sym),"#",1)
| (("skipU",sym),letter) -> (("skipU",sym),letter,1)

(* cross out corresponding letter in V *)
| (("crossV",sym),"X") -> (("crossV",sym),"X",1)
| (("crossV",sym),"#") -> (("rej","X"),"#",1)
| (("crossV",sym),"_") -> (("rej","X"),"_",0)
| (("crossV",sym),letter) -> if letter = sym then (("revVDone","X"),"X",0) else (("crossV",sym),letter,1)

(* go back and check for Xs along the way of V *) 
| (("revV",sym),"#") -> (("revU",sym),"#",0)
| (("revV",sym),letter) -> (("revV",sym),letter,0)

| (("revVDone",sym),"#") -> (("revUDone",sym),"#",0)
| (("revVDone",sym),letter) -> if letter = "X" then (("revVDone",sym),letter,0) else (("revV",sym),letter,0)

(* go back and check for Xs along the way of U *) 
| (("revU",sym),">") -> (("crossU",sym),">",1)
| (("revU",sym),letter) -> (("revU",sym),letter,0)

| (("revUDone",sym),">") -> (("finalSweep","X"),">",1)
| (("revUDone",sym),letter) -> if letter = "X" then (("revUDone",sym),letter,0) else (("revU",sym),letter,0)

(* final sweep to check that the whole string only contains Xs, #, >, _ *)
| (("finalSweep",sym),"_") -> (("acc","X"),"_",0)
| (("finalSweep",sym),letter) -> if exists letter (explode "X#>") then (("finalSweep",sym),letter,1) else (("rej","X"),letter,0)

(* wildcard *)
| (_,sym) -> (("rej","X"), sym, 1)
| (("", _), _) -> (("rej","X"), "X", 1)
)
};;

(* copied (and modified) from example *) 
let permutation = transform permutationTM (fun (x,y) -> x^"|"^y);;

(* permutation test *)
(**
(*acc*)
run permutation "#";;
run permutation "a#a";;
run permutation "obb#bob";;
run permutation "germany#mnayrge";;
(*rej*)
run permutation "a#";;
run permutation "#a";;
run permutation "aaa#aaaa";;
run permutation "a#a#aa";;
run permutation "hello";;
run permutation "ε";;
**) 



(* b *)

let copiesTM n = 
if n <= 0 then failwith "n cannot be less than 1 (select an n where n>0)"
else 
let explodeN = range n in
(* states = triple of state, copyVal, and currentN*)
{ states = triples ["start";"acc";"rej";"confirm";"rewind";"checkBeginning";"check#";"checkX";"findBack";"crossOff";"move";"nextCopy";"nextBack";"crossOff2"] ["-1";"0";"1"] explodeN;
input_alphabet = ["0";"1"];
tape_alphabet = (explode ">_X#01");
start = ("start","-1",0);
accept = ("acc","-1",0);
reject = ("rej","-1",0);
blank = "_";
left_marker = ">";
delta = (fun x -> 
match x with 
| (("start","-1",0),">") -> (("confirm","-1",0),">",1)

(* check for n-1 #s and legal alphabet overall *)
| (("confirm","-1",count),"0") -> (("confirm","-1",count),"0",1)
| (("confirm","-1",count),"1") -> (("confirm","-1",count),"1",1)
  (* update counter if # *)
| (("confirm","-1",count),"#") -> if count = n then (("rej","-1",0),"#",1) else (("confirm","-1",count+1),"#",1) 
| (("confirm","-1",count),"_") -> if  count = (n-1) then if n = 1 then (("acc","-1",0),"_",0) else 
(("rewind","-1",count),"_",0) else (("rej","-1",0),"_",0) 
| (("confirm","-1",count),sym) -> (("rej","-1",0),sym,1)

(* rewind to beginning and reset count *) 
| (("rewind",v,count),">") -> (("checkBeginning","-1",0),">",1)
| (("rewind",v,count),value) -> (("rewind",v,count),value,0)

(* check if first is # *)
| (("checkBeginning",v,count),"#") -> (("check#",v,count),"#",1)
| (("check#",v,count),"#") -> (("check#",v,count),"#",1)
| (("check#",v,count),"_") -> (("acc","-1",0),"_",0)
| (("check#",v,count),sym) -> (("rej","-1",0),sym,0)

(* check for Xs *)
| (("checkBeginning",v,count),"X") -> (("checkX",v,count),"X",1)
| (("checkX",v,count),"X") -> (("checkX",v,count),"X",1)
| (("checkX",v,count),"#") -> (("checkX",v,count),"#",1)
| (("checkX",v,count),"_") -> (("acc","-1",0),"_",0)
| (("checkX",v,count),sym) -> (("rej","-1",0),sym,0)

(* otherwise *)
| (("checkBeginning",v,count),value) -> (("findBack",v,count),value,1)

(* find back of copy and reverse direction to find first symbol to left of X or # *)
| (("findBack",value,count),"#") -> (("crossOff",value,count),"#",0)
| (("findBack",value,count),"X") -> (("crossOff",value,count),"X",0)
| (("findBack",value,count),sym) -> (("findBack",value,count),sym,1)

(* X out the symbol and save it as v *)
| (("crossOff",v,c),sym) -> (("move",sym,c+1),"X",1)

(* move onto the next copy in the string *) 
| (("move",v,c),"#") -> (("nextBack",v,c),"#",1)
| (("move",v,c),sym) -> (("move",v,c),sym,1)

(* find next back *)
| (("nextBack",value,count), "X") -> (("crossOff2",value,count),"X",0)
| (("nextBack",value,count), "#") -> (("crossOff2",value,count),"#",0)
| (("nextBack",value,count), "_") -> (("crossOff2",value,count),"_",0)
| (("nextBack",value,count), sym) -> (("nextBack",value,count),sym,1)

(* check that a symbol exists, matches v, and crosses symbol out - otherwise reject - if at end then rewind - otherwise look for end, cross it out, and continue *)
| (("crossOff2",v,c),"#") -> (("rej","-1",0),"#",1)
| (("crossOff2",v,c),sym) -> if sym = v then if c = (n-1) then (("rewind","-1",0),"X",0) else (("move",v,c+1),"X",1) else (("rej","-1",0),sym,0)

(* wildcard *)
| (("*",_,_),_) -> (("rej","0",n),"X",0)

)}
;;

let copies n = transform (copiesTM n) (fun (x,y,z) -> x^"|"^y^"|"^(string_of_int z));;


(* copies test *)
(** 
(*acc*)
run (copies 1) "1000";;
run (copies 1) "0101010101";;
run (copies 3) "##";;
run (copies 3) "01#01#01";;
run (copies 3) "1001#1001#1001";;
(*rej*)
run (copies 1) "0#1";;
run (copies 1) "10#10";;
run (copies 1) "10#10#10";;
run (copies 3) "#";;
run (copies 3) "###";;
run (copies 3) "0#0";;
run (copies 3) "10#10#10#10";;
**) 

