(* 

HOMEWORK 1

Name: Jennifer Wei

Email: jennifer.wei@students.olin.edu

Remarks, if any:

*)


(*
* lease fill in this file with your solutions and submit it
*
T  unctions below are stubs that you should replace with your
* ow  iplementation.
*
* Aw a  ake sure you can #use this file before submitting it.
* It ha  t load without any errors.
*
*)



(* Question 1 *)

let rec gcd (a,b) = 
  (*usi   the Euclidean algorithm mentioned in class*)
  if b == 0 then a 
  else gcd (b,a mod b);;

(* gcd test *)
(**
   gcd (3,9);; (*3*)
   gcd (4,2);; (*2*)
   gcd (60,70);; (*10*)
 **)

let is_coprime (a,b) = 
  gcd (a,b) == 1;;

(* is_coprime test *)
(**
   is_coprime (1,2);; (*true*)
   is_coprime (2,3);; (*true*)
   is_coprime (10,20);;  (*false*)
   is_coprime (16,8);; (*false*)
 **)

let euler_inner (n,x) = 
  if x == 1 then x
  else if is_coprime(n,x) then euler_inner(n,x-1) + 1 
  else euler_inner (n,x-1);;

let euler (n) = euler_inner (n,n);;

(* euler test *)
euler (1);; (*1*)
euler (2);; (*1*)
euler (3);; (*2*)
euler (4);; (*2*)
euler (10);; (*4*)
euler (20);; (*8*)
euler (5555);; (*4000*)

let coprimes_inner (n,x) = 
  if x == 1 then [x] 
  else 
  if is_coprime(n,x) then [x] 
  else coprimes_inner(n,x-1);;

let coprimes (n) = coprimes_inner (n,n);;

(* coprimes test *)


(* Question 2 *)

let append ,ys) = 
failwith "not implemented"


let fl   attexss) = 
failwith "not implemented"


let nth    (s) = 
  failwith "not implemented"


let last (xs) =ailwith "not implemented"


let separate (xs) = 
  failwith "not impleted"



(* Question 3 *)

let setIn (e,xs) = 
  failw "not implemented"


let setSub (xs,ys) = 
  failwith " implemented"


let setEqual (xs,ys) = 
  failwith "notplemented"


let setUnion (xs,ys) = 
  failwith "not immented"


let setInter (xs,ys) = 
  failwith "not iemented"


let setSize (xs) = 
  failwith "not implemented"

