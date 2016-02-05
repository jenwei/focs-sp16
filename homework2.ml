(* 

HOMEWORK 2

Name: Jennifer Wei

Email: jennifer.wei@students.olin.edu

Remarks, if any:
- Understood what all_strings is supposed to do and got it to work sketchily, but not 100% convinced that I can explain and visualize how it works
- langUnion returns the right language, but the strings are in a different order :(
- got langStar to work, but similar to all_strings, I don't completely understand it
- couldn't get dump to work - not sure what I did wrong
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



(* QUESTION 1 *)
(**
   (*if order doesn't matter*)
   let rec prependHelper (letter, lang, res) =
   match lang with
   | [] -> res
   | h::t -> prependHelper (letter, t, (letter^h)::res)
   ;;

   let prependOld (letter, lang) = prependHelper (letter, lang, []);;
 **)

(*if order matters*)
let rec prepend (letter, lang) =
  match lang with
    | [] -> []
    | h::t -> (letter^h)::prepend (letter, t)
;;

(* prepend test *)
(**
   prepend("",[]);;
   prepend("",["hello";"world"]);;
   prepend("test",[]);;
   prepend("test",["hello";"world"]);;
 **)


let rec concatenate (alphabet, lang) =
  match (alphabet, lang) with
    | [],[] -> []
    | [],[x] -> []
    | ss1,[] -> []
    | [],ss2 -> []
    | h::t,ss2 -> (prepend (h, ss2) @ concatenate (t, ss2))
;;

(* concatenate test *)
(**
   concatenate([],[]);;
   concatenate([],["hello";"world"]);;
   concatenate(["a"],["hello";"world"]);;
   concatenate(["a";"b"],["hello";"world"]);;
   concatenate(["a";"b"],[]);;
   concatenate(["a";"b"],["hello"]);;
 **)


let rec all_stringsHelper (alphabet, n) =
  match n with
    | 0 -> []
    | n -> alphabet @ concatenate (alphabet, all_stringsHelper (alphabet, n-1))
;;

let all_strings (alphabet, n) =
  [""] @ all_stringsHelper (alphabet,  n)
;;

(* all_strings test *)
(**
   all_strings([],4);;
   all_strings(["a"],4);;
   all_strings(["a";"b"],4);;
   all_strings(["a";"b";"c"],4);;
   all_strings(["a";"b"],1);;
 **)


(* QUESTION 2 *)

let rec restrict (xs,n) = 
  match xs with
    | [] -> []
    | h::t -> if ((String.length h) <= n) then h::restrict (t, n) else (restrict (t, n))
;;

(* restrict test *)
(**
   restrict([],4);;
   restrict(["a";"b"],4);;
   restrict(["a";"b"],0);;
   restrict(["a";"b"],1);;
   restrict(["a";"b";"abc"],1);;
   restrict(["a";"b";"abc"],2);;
   restrict(["a";"b";"abc"],3);;
 **)


(* took this code from hw1 *)
let rec setIn (e,xs) = 
  match xs with
    | [] -> false
    | h::t -> if h = e then true else setIn (e,t)
;;

(* took this code from hw1 *)
let rec setUnionHelper (xs,ys,ls) = 
  match (xs,ys) with
    | [],[] -> ls
    | xh::xt,ys -> if setIn (xh,ys) then setUnionHelper (xt,ys,ls) else setUnionHelper (xt,xh::ys,ls)
    | [],yh::yt -> if setIn (yh,yt) then setUnionHelper ([],yt,ls) else setUnionHelper ([],yt, yh::ls)
;;

(* took this code from hw1 *)
let setUnion (xs,ys) = setUnionHelper (xs,ys,[]);;

let langUnion (xs,ys,n) = 
  setUnion (restrict (xs, n), restrict (ys, n))
;;

(* langUnion test *) 
(**
   langUnion([],[],4);;
   langUnion(["a";"b"],["c";"d"],4);;
   langUnion(["a";"b"],["abc";"abcd";"abcde"],4);;
   langUnion(["abc";"abcd";"abcde"],["a";"b"],4);;
   langUnion(["abc";"abcd";"abcde"],[],4);;
   langUnion([],["abc";"abcd";"abcde"],4);;
 **)


let rec langConcat (xs,ys,n) = 
  match xs with
    | [] -> []
    | h::t -> restrict (prepend (h, ys) @ langConcat(t, ys, n), n)
;;

(* langConcat test *)
(**
   langConcat([],[],4);;
   langConcat(["a";"b"],[],4);;
   langConcat([],["c";"d"],4);;
   langConcat(["a";"b"],["c";"d"],4);;
   langConcat(["ab";"abb"],["c";"cc";"ccc"],4);;
 **)


(* referenced all_strings*)
let rec langStarHelper (xs,n) = 
  match n with
    | 0 -> []
    | n -> xs @ langConcat (xs, langStarHelper(xs, n-1), n)
;;

let langStar (xs, n) = 
  [""] @ langStarHelper (xs,n)
;;

(* langStar test *)
(**
   langStar([],4);;
   langStar(["a"],4);;
   langStar(["a";"b"],4);;
   langStar(["a";"b";"c"],4);;
   langStar(["a";"bc"],4);;
   langStar(["a";"bc";"def"],4);;
 **)

(* QUESTION 3 *)


(* some helper code -- vaguely advanced OCaml in here, but barely *)

type re = Empty | Unit | Letter of string | Plus of re * re | Times of re * re | Star of re

let lang (s,n) = 
  let fromChar c = String.make 1 c in
  let explode s = 
    let rec loop i result = 
      if i < 0 then result
      else loop (i-1) (s.[i]::result) in
      loop (String.length s - 1) []  in
  (* Grammar: 
   *
   * R ::= R1 + R
   *       R1
   * 
   * R1 ::= R2 R1
   *        R2
   * 
   * R2 ::= R3*
   *        R3
   * 
   * R3 ::= a
   *        1
   *        0 
   *        ( R )
  *)
  let isalpha = function 'A'..'Z'|'a'..'z' -> true | _ -> false in
  let expect c cs = 
    match cs with 
        f::cs when f = c -> Some cs
      | _ -> None in
  let expect_alpha cs = 
    match cs with
        f::cs when isalpha f -> Some (f,cs)
      | _ -> None  in
  let rec parse_R cs = 
    match parse_R1 cs with
        None -> None
      | Some (r1,cs) -> 
          (match expect '+' cs with
              None -> Some (r1,cs)
            | Some cs -> 
                (match parse_R cs with
                    None -> None
                  | Some (r2,cs) -> Some (Plus(r1,r2),cs)))
  and parse_R1 cs = 
    match parse_R2 cs with
        None -> None
      | Some (r1,cs) -> 
          (match parse_R1 cs with
              None -> Some (r1,cs)
            | Some (r2,cs) -> Some (Times(r1,r2),cs))  
  and parse_R2 cs = 
    match parse_R3 cs with
        None -> None
      | Some (r1,cs) -> 
          (match expect '*' cs with
              None -> Some (r1,cs)
            | Some cs -> Some (Star(r1),cs))
  and parse_R3 cs = 
    match expect_alpha cs with
        Some (a,cs) -> Some (Letter(fromChar(a)),cs)
      | None -> 
          (match expect '1' cs with
              Some cs -> Some (Unit, cs)
            | None -> 
                (match expect '0' cs with
                    Some cs -> Some (Empty,cs)
                  | None -> parse_parens cs))
  and parse_parens cs = 
    match expect '(' cs with
        None -> None
      | Some cs -> 
          (match parse_R cs with
              None -> None
            | Some (r,cs) -> 
                (match expect ')' cs with
                    None -> None
                  | Some cs -> Some (r,cs)))  in
  let parse s = 
    let cs = explode s in
      match parse_R cs with
          Some (re,[]) -> re
        | _ -> failwith ("Cannot parse "^s)  in
  let rec eval re = 
    match re with
        Empty -> []
      | Unit -> [""]
      | Letter (a) -> [a]
      | Plus (r1,r2) -> langUnion(eval r1,eval r2,n)
      | Times (r1,r2) -> langConcat(eval r1,eval r2,n)
      | Star r -> langStar(eval r,n)  in
    eval (parse s)

let dump l = 
  List.iter (fun s -> match s with "" -> print_string "  <empty>\n" 
                                 | s -> print_string ("  "^s^"\n")) l

let regexp_a = "(a+b)(a+b)(a+b)";;
(**(lang(regexp_a,6));;**)
(*language consisting of all strings over the alphabet {a,b} of length 3*)

let regexp_b = "((a+b)(a+b)(a+b))*";;
(*language consisting of all strings over the alphabet {a,b} of length multiple of 3*)

let regexp_c = "(b)*a(b)*";;
(*language consisting of all strings over the alphabet {a,b} with exactly one a in them*)

let regexp_d = "b*ab*(ab*ab*)*";;
(*language consisting of all strings over the alphabet {a,b} with an odd number of a's in them*)

let regexp_e = "(a+ba)*";;
(*language consisting of all strings over the alphabet {a,b} in which every b is immediately followed by an a*)
