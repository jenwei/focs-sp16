(* type for arithmetic expressions *)

type aexp =
    | Number of int
    | Plus of aexp * aexp
    | Times of aexp * aexp
    | Minus of aexp


(* size (number of nodes) *)

let rec size t =
    match t with
    | Number (i) -> 1
    | Plus (e,f) -> 1+size(e)+size(f)
    | Times (e,f) -> 1+size(e)+size(f)
    | Minus (e) -> 1+size(e)

(* height *)

let rec height t =
    match t with
    | Number (i) -> 1
    | Plus (e,f) -> 1 + (if (height e) > (height f) then (height e) else (height f))
    | Times (e,f) -> 1 + (if (height e) > (height f) then (height e) else (height f))
    | Minus (i) -> 1 + (height i)

(* all_numbers *)

let rec all_numbers t =
    match t with
    | Number (i) -> [i]
    | Plus (e,f) -> (all_numbers e)@(all_numbers f)
    | Times (e,f) -> (all_numbers e)@(all_numbers f)
    | Minus (e) -> (all_numbers e)

(* evaluate *)

let rec eval t =
    match t with
    | Number (i) -> i
    | Plus (e, f) -> eval(e) + eval(f)
    | Times (e, f) -> eval(e) * eval(f)
    | Minus (e) -> -(eval(e))

let test = Times (Plus (Number 10, Number 20), Number 30)
