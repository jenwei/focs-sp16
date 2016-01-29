(* 

HOMEWORK 1

Name: Jennifer Wei

Email: jennifer.wei@students.olin.edu

Remarks, if any:
Worked with Pratool on debugging/talking through the last three problems (he was helping me understand)
Also talked to Dennis about debugging
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
  if b = 0 then a 
  else gcd (b,a mod b)
;;

(* gcd test *)
(**
   gcd (3,9);; (*3*)
   gcd (4,2);; (*2*)
   gcd (60,70);; (*10*)
 **)



let is_coprime (a,b) = 
  gcd (a,b) = 1
;;

(* is_coprime test *)
(**
   is_coprime (1,2);; (*true*)
   is_coprime (2,3);; (*true*)
   is_coprime (10,20);;     (*false*)
   is_coprime (16,8);; (*false*)
 **)



let rec eulerHelper (n,x) = 
  if x = 1 then x
  else if is_coprime (n,x) then eulerHelper (n,x-1) + 1
  else eulerHelper (n,x-1)
;;

let euler (n) = eulerHelper (n,n);;

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



let rec coprimesHelper_old (n,x) = 
  (*http://stackoverflow.com/questions/6732524/what-is-the-easiest-way-to-add-an-element-to-the-end-of-the-list*)
  if x = 1 then [x]
  else if is_coprime (n,x) then coprimesHelper_old (n,x-1) @ [x]
  else coprimesHelper_old (n,x-1)
;;

let coprimes_old (n) = coprimesHelper_old (n,n);;



let rec coprimesHelper(n,x) = 
  if x = n then 
    if is_coprime (n,x) then [x] else []
  else if is_coprime (n,x) then x::coprimesHelper (n,x+1) 
  else coprimesHelper (n,x+1);;

let coprimes (n) = coprimesHelper (n,1);;

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
    | h::t -> h::append (t,ys)
    | [] -> ys
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
    | h::t -> append (h,flatten (t))
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
    | h::t -> last (t)
;;

(* last test *)
(**
   last ([1]);;
   last ([1;2]);;
   last ([1;2;3;4;5]);;
   last (["a";"b";"c"]);;
   last ([]);;
 **)



let rec nth (n,xs) = 
  match (n,xs) with
    | (0,h::t) -> h
    | (n,[]) -> failwith "out of bounds"
    | (n,h::t) -> nth (n-1,t)
;;

(* nth test *)
(**
   nth (0,["a";"b";"c"]);;
   nth (1,["a";"b";"c"]);;
   nth (2,["a";"b";"c"]);;
   nth (3,["a";"b";"c"]);;
   nth (0,[]);;
 **)



let separateHelper (a,b,ls) = 
  match ls with
    | (x,y) -> (a::x,b::y)
;;

let rec separate (xs) = 
  match xs with
    | [] -> ([],[])
    | (a,b)::t -> separateHelper (a,b,(separate (t)))
;;

(* separate test *)
(**
   separate [];;
   separate [(1,2)];;
   separate [(1,2);(3,4)];;
   separate [(1,"a");(2,"b");(3,"c")];;
 **)



(* Question 3 *)

let rec setIn (e,xs) = 
  match xs with
    | [] -> false
    | h::t -> if h = e then true else setIn (e,t)
;;

(* setIn test *)
(**
   setIn (1,[]);;
   setIn (1,[2;3]);;
   setIn (1,[3;4;4;1;1;]);;
   setIn ("a",["b";"a";"b"]);;
 **)



let rec setSub (xs,ys) = 
  match xs with
    | [] -> true
    | h::t -> if setIn (h,ys) then setSub (t,ys) else false
;;

(* setSub test *)
(**
   setSub ([],[]);;
   setSub ([],[1;1;1]);;
   setSub ([1],[1;1;1]);;
   setSub ([1;1;],[1;1;1]);;
   setSub ([1;1;],[2;3]);;
   setSub (["a"],["a";"b"]);;
 **)



let setEqual (xs,ys) = 
  setSub (xs,ys) && setSub (ys,xs)
;;

(* setEqual test *)
(**
   setEqual ([],[]);;
   setEqual ([1],[1;1;1]);;
   setEqual ([1;1;1],[1;1]);;
   setEqual ([1;2],[2;1]);;
   setEqual ([1;2],[1;2;3]);;
 **)



let rec setUnionHelper (xs,ys,ls) = 
  match (xs,ys) with
    | [],[] -> ls
    | xh::xt,ys -> if setIn (xh,ys) then setUnionHelper (xt,ys,ls) else setUnionHelper (xt,xh::ys,ls)
    | [],yh::yt -> if setIn (yh,yt) then setUnionHelper ([],yt,ls) else setUnionHelper ([],yt, yh::ls)
;;

let setUnion (xs,ys) = setUnionHelper (xs,ys,[]);;

(* setUnion test *)
setUnion ([1],[2]);; (*true*)
setUnion ([],[1;1]);; (*true*)
setUnion ([1;2;3],[4;5;6]);; (*true*)
setUnion ([1],[2]);; (*false*)



let rec setInterHelper (xs,ys,ls) = 
  match (xs) with
    | []-> ls
    | xh::xt -> if setIn (xh,ys) then setInterHelper (xt,ys,xh::ls) else setInterHelper (xt,ys,ls)
;;

let setInter (xs,ys) = setInterHelper (xs,ys,[]);;

(* setInter test *)
(**
   setEqual (setInter ([],[]), []);;
   setEqual (setInter ([1;2],[2;3]), [2]);;
   setEqual (setInter ([1;2;3],[3;3;2;2]), [2;3]);;
   setEqual (setInter ([1;2;3],[]), []);;
   setEqual (setInter (["a";"b"],["c";"b"]), ["b"]);;
   setEqual (setInter ([1;2],[2]), [1]);;
 **)



let rec setSizeCounter (xs,count) =
  match xs with
    | [] -> count
    | h::t -> setSizeCounter(t,count+1)
;;

let setSize (xs) = setSizeCounter (setUnion ([],xs),0);;

(* setSize test *)
(**
   setSize [];;
   setSize [1];;
   setSize [1;2;3];;
   setSize [1;1;1;1;2;2;2;2;3;3;3;3;4;4;4;4];;
 **)
