(* 

HOMEWORK 10

Name: Jennifer Wei

Email: jennifer.wei@students.olin.edu

Remarks, if any:
- Received some guidance/tips from Pratool (and also got in-class notes from Pratool)
- Considered taking advantage of pre/post/in order, but decided to go with an easier 
  less intense and efficient version
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


type 'a bintree =
  | Empty
  | Node of 'a * 'a bintree * 'a bintree


let sample = Node(10,Node(3,Node(7,Empty,Empty),
                            Node(5,Empty,Empty)),
                     Node(6,Node(99,Empty,
                                 Node(66,Empty,Empty)),
                          Empty))


(* Printing an integer binary tree *)

let pbt bt =
  let rec loop bt depth = 
    match bt with
    | Empty -> ()
    | Node(n,left,right) ->
	(loop right (depth^"    ");
         print_endline (depth^(string_of_int n));
         loop left (depth^"    ")) in
  loop bt ""





(* Q1 *)

(* a *)
let rec size t = 
  match t with 
    | Empty -> 0
    | Node(n,left,right) -> 1 + (size left) + (size right)
;;

let rec sum t = 
  match t with 
    | Empty -> 0
    | Node(n,left,right) -> n + (sum left) + (sum right)
;;

let rec height t = 
  match t with 
    | Empty -> 0
    | Node(n,left,right) -> 1 + (if (height left) > (height right) then (height left) else (height right))
;;

(* b *)
let rec fringe t = 
    match t with 
    | Empty -> []
    | Node(n,left,right) -> if (fringe(left) = []) && (fringe(right) = []) then [n] else fringe(left) @ fringe(right)
;;

(* c *)
let rec map f t = 
  match t with 
    | Empty -> Empty
    | Node(n,left,right) -> Node((f n),(map f left),(map f right))
;;

(* d *)
let rec fold f t b = 
    match t with 
    | Empty -> b
    | Node(n,left,right) -> f n (fold f left b) (fold f right b)
;;

let preorder t = 
  fold (fun v l r -> ([v] @ l @ r)) t []
;;

let postorder t = 
    fold (fun v l r -> (l @ r @ [v] )) t []
;;


let inorder t = 
    fold (fun v l r -> (l @ [v] @ r)) t []
;;

(* e *)
let rec bst_insert t x = 
    match t with 
    | Empty -> Node(x,Empty,Empty)
    | Node(n,left,right) -> if (x > n) then Node(n,left,(bst_insert right x)) else Node(n,(bst_insert left x),right)
;;


let rec bst_lookup t x = 
    match t with 
    | Empty -> false
    | Node(n,left,right) -> if (x = n) then true else
      if (x > n) then (bst_lookup right x) else (bst_lookup left x)
;;


let rec bstify t =  List.fold_right (fun x t -> (bst_insert t x)) (inorder t) Empty
;;


let avl_insert t x = failwith ("avl_insert not implemented or attempted")
