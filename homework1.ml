(* 

HOMEWORK 1

Name: Jennifer Wei

Email: jennifer.wei@students.olin.edu

Remarks, if any:

*)


(*
* Please fill in this file with your solutions and submit it
*
The functions below are stubs that you should replace with your
* own implementation.
*
* Always make sure you can #use this file before submitting it.
* It has to load without any errors.
*
*)



(* Question 1 *)

let rec gcd (a,b) = 
  (*using the Euclidean algorithm mentioned in class*)
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
   is_coprime (10,20);;     (*false*)
   is_coprime (16,8);; (*false*)
 **)


let euler_inner (n,x) = 
  if x == 1 then x
  else if is_coprime (n,x) then euler_inner (n,x-1) + 1 
  else euler_inner (n,x-1);;

let euler (n) = euler_inner (n,n);;

(* euler test *)
(**
   euler (1);; (*1*)
   euler (2);; (*1*)
   eule   r (3);; (*2*)
   euler (4);; (*2*)
   euler (10);; (*4*)
   euler (20);; (*8*)
   euler (5555);; (*4000*)
 **)


let rec coprimes_inner_old (n,x) = 
  (*http://stackoverflow.com/questions/6732524/what-is-the-easiest-way-to-add-an-element-to-the-end-of-the-list*)
  if x == 1 then [x]
  else if is_coprime (n,x) then coprimes_inner_old (n,x-1) @ [x]
  else coprimes_inner_old (n,x-1);;

let coprimes_old (n) = coprimes_inner_old (n,n);;



let rec coprimes_inner (n,x) = 
  if x == n then 
    if is_coprime (n,x) then [x] else []
  else if is_coprime (n,x) then x :: coprimes_inner (n,x+1) 
  else coprimes_inner (n,x+1);;

let coprimes (n) = coprimes_inner (n,1);;

(* coprimes test *)
(**
   coprimes (1);;
   coprimes (2);;
   coprimes (3);;
   coprimes (4);;
   coprimes (10);;
   coprimes (20);;
   coprimes (5555);;
 **)



(* Question 2 *)

let rec append (xs,ys) = 
  (*http://rigaux.org/language-study/syntax-across-languages-per-language/OCaml.html for pattern matching syntax*)
  (*https://realworldocaml.org/v1/en/html/lists-and-patterns.html for extracting data from a list*)
  match xs with 
    | h :: t -> h :: append (t,ys)
    |[] -> ys
;;

(* append test *)
(**
   append ([],[]);;
   append ([1],[]);;
   append ([],[1]);;
   append ([1;2;3],[4;5;6]);;
   append (["a"],["b"]);;
 **)



let rec flatten (xss) = 
  match xss with
    | [] -> []
    | h :: t -> append (h, flatten (t))
;;

(* flatten test *)
(**
   flatten [];;
   flatten [[1;2;3]];;
   flatten [[1;2;3];[4;5;6]];;
   flatten [[1;2;3];[4;5;6];[7;8]];;
   flatten [[1;2;3];[];[7;8]];;
   flatten [["a"];["b"]];;
 **)



let rec last (xs) = 
  match xs with 
    | [] -> failwith "empty - no last element"
    | [x] -> x
    | h :: t -> last (t)
;;

(* last test *)
(**
  last ([1]);;
  last ([1;2]);;
  last ([1;2;3;4;5]);;
  last (["a";"b";"c"]);;
  last ([]);;
 **)



let nth (n,xs) = 
  failwith "not implemented"


let separate (xs) = 
  failwith "not implemented"



(* Question 3 *)

let setIn (e,xs) = 
  failwith "not implemented"


let setSub (xs,ys) = 
  failwith "not implemented"


let setEqual (xs,ys) = 
  failwith "not implemented"


let setUnion (xs,ys) = 
  failwith "not implemented"


let setInter (xs,ys) = 
  failwith "not implemented"


let setSize (xs) = 
  failwith "not implemented"

