let rec squares(arr) =
  match arr with
    [] -> []
  | hd::tl -> (hd*hd)::squares(tl)

let rec diags(arr) =
  match arr with
    [] -> []
  | hd::tl -> (hd,hd)::diags(tl)

let rec lengths(arr) = 
  match arr with
    [] -> []
  | hd::tl -> List.length(hd)::lengths(tl)

(* Can generalize the above three to a mpa with input function body*)
let rec map(fn,xs) = 
  match xs with 
    [] -> []
  | x::xs' -> (fn x)::map(fn,xs')

(*Some functions like above using map. fun x->x+1 sort of structure
 * can implement functions without labeling them explicitly*)
let diags3(xs) =
  map((fun x -> (x,x,x)), xs)

let thirds(xs) = 
  map((fun (a,b,c) -> c), xs)

let distribute(h, xs) = 
  map((fun x -> (h,x)), xs)

(*alternatively*)
let create_mkPair y = (fun x->(y,x))

let distribute2 (y, xs) =
  map (create_mkPair y, xs)

(*Multiple argument functions? *)
let add x y = x + y
(* says, construct a function f1 that takes x and expects y, and then
  * construct a function f2 that takes f1 and expects y. 
  * Functions cascade into new functions!*)

(*It's possible to partially apply functions and get functions back as
 * return value that expect the rest of the arguments. *)
let increment = add 1
(*then try increment 5 *)

let rec removeEmpty arr =
  match arr with 
    [] -> []
  | a::b -> if a = [] then removeEmpty b else a::removeEmpty b

(*filter generalizes the above, to use the function cond as the condition for
 * evaluating the conditional *)
let rec filter cond arr =
  match arr with 
    [] -> []
  | a::b -> if (cond a) then filter cond b else a::filter cond b

let odd n = (n mod 2 = 1)

(* Try filter odd [1;2;3;4] *)
(* Map cannot build filter, unless you modify map so that it 
 * uses append instead of adding a new head.
 * In other words, map can't drop elements and []::[list...] fails
 * while []@[list...] will succeed*)

let rec map_append f xs =
  match xs with 
    [] -> []
  | x::xs' -> (f x)@(map_append f xs')

(* Connects lists to lists rather than items to list. WOrks because there
 * exists an empty list, but no empty item *)

let rec flatten xs =
  map_append (fun x->x) xs

let rec filter2 cond xs =
  map_append (fun x -> if (cond x) then [x] else []) xs

(* Now generalize map? *)
let rec map_general1 comb f xs =
  match xs with 
    []->[]
  | x::xs'-> comb (f x) (map_general1 comb f xs')

let map = map_general1 (fun x y -> x::y) f xs

let rec map_general2 comb xs = 
  match xs with 
    [] -> []
  | x::xs' -> comb x (map_general2 comb xs')

let map f xs = map_general2 (fun x y -> (f x)::y) xs

(*Map general cannot perform a reduction operation like sum.
 * sum returns a value, not a list.*)

let rec sum_general comb xs = 
  match arr with
    [] -> []
  | x::xs' -> comb x (sum_general comb xs')

(* The most general function for doing something
 * over a list. *)
let rec fold_right comb xs base =
  match xs with 
    [] -> base
  | x::xs' -> comb x (general comb xs' base)

