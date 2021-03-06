(* 

HOMEWORK 8

Name: Jennifer Wei

Email: jennifer.wei@students.olin.edu

Remarks, if any:
- Received some tips from Dennis and Pratool about approaching Q2 and Q3
- Received a protip from Dennis about how to indent the lambda expression to look more like a function/code
- Any suggestions on debugging lambda terms? Tried excessive indenting but that still took a while (esp.  
	Q2c - spent forever debugging - I think the resolution was that I put parentheses around too many lines 
	and didn't separate the conditions correctly)
- Received help from Dennis on Q3 about how empty and cons should work
- Spent a while debugging Q3d, only to realize that I forgot to add /append to the head T__T
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
* The implementation of parsing and simplication of lambda terms
*
* I've isolated it away into a module that we don't need to peer into
*
*)

module LambdaTerms = struct

  type lterm = 
        LIdent of string
      | LLam of string * lterm
      | LApp of lterm * lterm

  let fresh =
    let tag = ref 0 in
      fun id ->
        let new_id = id^"_"^(string_of_int (!tag)) in
          ( tag := (!tag) + 1; new_id)

  let lexer = Genlex.make_lexer ["(";")";".";"/"]

  let lex s = 
    let str = lexer (Stream.of_string s)  in
    let rec loop () = 
      match (Stream.peek str) with
        | None -> []
        | Some _ -> let elt = Stream.next str in elt::(loop())  in
      loop ()

  let expect elt cs = 
    match cs with
      | f::cs when f = elt -> Some cs
      | _ -> None

  let expect_ident cs = 
    match cs with
      | (Genlex.Ident id)::cs -> Some (id,cs)
      | _ -> None

  let rec parse_term cs = 
    match parse_ident_terms cs with
      | Some x -> Some x
      | None -> 
          (match parse_lambda cs with
            |	Some x -> Some x
            |	None ->
                (match parse_group_terms cs with
                  | Some x -> Some x
                  | None -> 
                      (match parse_ident cs with
                        |	Some x -> Some x
                        |	None -> 
                            (match parse_group cs with
                              | Some x -> Some x
                              | None -> None))))

  and parse_ident_term cs = 
    match parse_ident cs with
      | None -> None
      | Some (term1,cs) -> 
          (match parse_term cs with
            |	None -> None
            |	Some (term2,cs) -> Some (LApp(term1,term2),cs))

  and parse_ident_terms cs =    (* ident term+ *)
    match parse_ident cs with
      | None -> None
      | Some (term1,cs) -> 
          (match parse_terms cs with
            |	None -> None
            |	Some (f,cs) -> Some (f term1,cs))

  and parse_group_terms cs =    (* group term+ *)
    match parse_group cs with
      | None -> None
      | Some (term1,cs) ->
          (match parse_terms cs with
            |	None -> None
            |	Some (f,cs) -> Some (f term1, cs))

  and parse_terms cs = 
    match parse_ident cs with
      | Some (term1,cs) -> 
          (match parse_terms cs with
            |	None -> Some ((fun t -> LApp(t,term1)),cs)
            |	Some (f,cs) -> Some ((fun t -> f (LApp (t,term1))),cs))
      | None-> 
          (match parse_group cs with
            |	Some (term1,cs) -> 
                (match parse_terms cs with
                  | None -> Some ((fun t -> LApp(t,term1)),cs)
                  | Some (f,cs) -> Some ((fun t -> f (LApp (t,term1))),cs))
            |	None -> None)


  and parse_ident cs =
    match expect_ident cs with
      | None -> None
      | Some (id,cs) -> Some (LIdent id,cs)

  and parse_lambda cs = 
    match expect (Genlex.Kwd "/") cs with
      | None -> None
      | Some cs -> 
          (match expect_ident cs with
            |	None -> None
            |	Some (id,cs) -> 
                (match expect (Genlex.Kwd ".") cs with
                  | None -> None
                  | Some cs -> 
                      (match parse_term cs with
                        |	None -> None
                        |	Some (term,cs) -> Some (LLam (id,term),cs))))

  and parse_group_term cs =
    match parse_group cs with
      | None -> None
      | Some (term1,cs) ->
          (match parse_term cs with
            |	None -> None
            |	Some (term2,cs) -> Some (LApp (term1,term2),cs))

  and parse_group cs =
    match expect (Genlex.Kwd "(") cs with
      | None -> None
      | Some cs ->
          (match parse_term cs with
            |	None -> None
            |	Some (term,cs) ->
                (match expect (Genlex.Kwd ")") cs with
                  | None -> None
                  | Some cs -> Some (term,cs)))

  let parse str = 
    match parse_term (lex str) with
      | Some (term,[]) -> term
      | _ -> failwith ("Cannot parse "^str)

  let rec pp term = 
    match term with
      | LIdent x -> x
      | LLam (x,t) -> "/"^x^"."^(pp t)
      | LApp (t1,t2) -> 
          let t1' = (match t1 with
                      | LLam _ -> "("^(pp t1)^")"
                      | _ -> pp t1)  in
          let t2' = (match t2 with
                      | LApp _ -> "("^(pp t2)^")"
                      | LLam _ -> "("^(pp t2)^")"
                      | _ -> pp t2)  in
            t1'^" "^t2'


  let rec rename term old nw = 
    match term with
      | LIdent x when x = old -> LIdent nw
      | LIdent x -> LIdent x
      | LLam (x,t) when x = old  -> LLam (x,t)
      | LLam (x,t) -> LLam (x, rename t old nw)
      | LApp (t1,t2) -> LApp (rename t1 old nw,
                              rename t2 old nw)

  let rec fv m = 
    match m with
      | LIdent x -> [x]
      | LLam (x,t) -> List.filter (fun y -> x <> y) (fv t)
      | LApp (t1,t2) -> (fv t1) @ (fv t2)

  let rec substitute m s n = 
    match m with
      | LIdent x when x = s -> n
      | LIdent x -> LIdent x
      | LLam (x,t) when x = s -> LLam (x,t)
      | LLam (x,t) when List.mem x (fv n) -> 
          let x_ = fresh x in
            substitute (LLam (x_,rename t x x_)) s n
      | LLam (x,t) -> LLam (x,substitute t s n)
      | LApp (t1,t2) -> LApp (substitute t1 s n,
                              substitute t2 s n)

  let rec reduce term = 
    match term with
      | LIdent s -> None
      | LLam (s,term) -> 
          (match reduce term with
            |	None -> None
            |	Some t -> Some (LLam(s,t)))
      | LApp (LLam (s,term1),term2) -> 
          Some (substitute term1 s term2)
      | LApp (term1,term2) -> 
          (match reduce term1 with
            |	None -> (match reduce term2 with
                        | None -> None
                        | Some t2 -> Some (LApp (term1,t2)))
            |	Some t1 -> Some (LApp (t1,term2)))

  let expand_all defs = 
    let expand defs t = List.fold_left (fun r (n,d) -> substitute r n d) t defs in
    let rec loop done_defs defs = 
      match defs with
        | [] -> done_defs
        | (name,df)::dfs -> loop ((name,expand done_defs df)::done_defs) dfs  in
      loop [] defs

  let threshold = 5000

  let simplify' print_term defs term = 
    let term = parse term  in
    let expand defs t = List.fold_left (fun r (n,d) -> substitute r n d) t defs in
    let defs = expand_all (List.map (fun (n,d) -> (n,parse d)) defs) in
    let term = expand defs term  in
    let rec loop n term =  
      let _ = print_term (" = "^(pp term)) in
        if n > threshold
        then failwith ("failed to find normal form after "^(string_of_int threshold)^" simplifications")
        else 
          match (reduce term) with
            | None -> pp term
            |	Some term -> loop (n+1) term  in
      match reduce term with
        | None -> let _ = print_endline "Term already in normal form" in (pp term)
        | Some next -> let _ = print_term ("   "^(pp term))  in loop 0 next

end




(*
* Simplification of lambda terms
*
* One version that simply returns the result
* One version that prints all intermediate terms
*
*)

let simplify_verbose defs term = LambdaTerms.simplify' print_endline defs term

let simplify defs term = LambdaTerms.simplify' (fun t -> ()) defs term


(* 
* The default definitions from class
*
*)

let default_defs = [ ("true","/x./y.x");
                     ("false","/x./y.y");
                     ("if","/c./x./y.c x y");
                     ("and","/b./c.b c false");
                     ("or","/b./c.b true c");
                     ("not","/b.b false true");
                     ("_0","/f./x.x");
                     ("_1","/f./x.(f x)");
                     ("_2","/f./x.(f (f x))");
                     ("_3","/f./x.(f (f (f x)))");
                     ("_4","/f./x.(f (f (f (f x))))");
                     ("_5","/f./x.(f (f (f (f (f x )))))");
                     ("succ","/n./f./x.(n f) (f x)");
                     ("plus","/m./n./f./x.(m f) (n f x)");
                     ("times","/m./n./f./x.m (n f) x");
                     ("iszero","/n.n (/x.false) true");
                     ("pred","/n./f./x.n (/g.(/h.h (g f))) (/u.x) (/u.u)");
                     ("Y","/f.(/x.f (x x)) (/x.f (x x))");
                     ("fact","Y (/fact./n.(iszero n) _1 (times n (fact (pred n))))") ]


(*************************************************************
 * Question 1
 *
 *************************************************************)

(*
* By default, all of these are implemented as identifier "not_implemented"
* 
* Just replace that with your own definition
*
*)

(*minus takes two natural numbers m and n and returns the natural number m-n*)
let minus = ("minus","/m./n.n pred m") (* this one was given to us *);;

let geq = ("geq","/m./n.iszero (minus n m)") (* adapted this from the reading you gave us *);;

(*
simplify q1_defs "geq _0 _0";;
simplify q1_defs "geq _1 _0";;
simplify q1_defs "geq _1 _3";;
simplify q1_defs "geq _1 (plus _1 _1)";;
*)

let eq = ("eq","/m./n.and (geq m n) (geq n m)") (* adapted this from the reading you gave us *);;

(*
simplify q1_defs "eq _0 _0";;
simplify q1_defs "eq _2 _2";;
simplify q1_defs "eq _0 _2";;
simplify q1_defs "eq _3 _2";;
*)

let pair = ("pair","/m./n./p.p m n") (* adapted this from the reading you gave us *);;

let match_pair = ("match_pair","/pair./f.pair f");;

(*
simplify q1_defs "match_pair (pair x y) f";;
simplify q1_defs "match_pair (pair _1 _2) plus";;
simplify q1_defs "match_pair (pair true false) or";;
simplify q1_defs "match_pair (pair true false) and";;
simplify q1_defs "(/p.match_pair p (/x./y.times (succ x) (succ y))) (pair _1 _2)";;
simplify q1_defs "(/p.match_pair p (/x./y.eq y (succ x))) (pair _1 _2)";;
simplify q1_defs "(/p.match_pair p (/x./y.eq y (succ x))) (pair _1 _3)";;
*)

let fst = ("fst","/pair.pair (/x./y.x)") (* adapted this from the reading you gave us *);;

let snd = ("snd","/pair.pair (/x./y.y)") (* adapted this from the reading you gave us *);;

(*
simplify q1_defs "fst (pair a b)";;
simplify q1_defs "snd (pair a b)";;
simplify q1_defs "fst (pair _3 (plus _1 _1))";;
simplify q1_defs "snd (pair _3 (plus _1 _1))";;    
*)

let update_fst = ("update_fst","/op./v.(pair v (snd op))");; (* op = original pair, v = value *)

let update_snd = ("update_snd","/op./v.(pair (fst op) v)");; (* yay anonymous functions *)

(*
simplify q1_defs "fst (update_fst (pair a b) c)";;
simplify q1_defs "snd (update_fst (pair a b) c)";;
simplify q1_defs "fst (update_snd (pair a b) c)";;
simplify q1_defs "snd (update_snd (pair a b) c)";;
*)

(* 
* Make sure all your definitions are added to this -- this is what I'll be testing 
*)

let q1_defs = default_defs @ [ minus; geq; eq; pair; match_pair; fst; snd; update_fst; update_snd]



(*************************************************************
 * Question 2
 *
 *************************************************************)

(*
* By default, all of these are implemented as identifier "not_implemented"
* 
* Just replace that with your own definition
*
*)

(*int takes a natural number and returns a postive number*)
let int = ("int","/n.pair true n");;

(*
simplify q2_defs "fst (int _0)";;
simplify q2_defs "fst (int _1)";;
simplify q2_defs "snd (int _1)";;
simplify q2_defs "snd (int _2)";;
*)

(*neg_int takes an encoded integer and returns its negation*)
let neg_int = ("neg_int","/n.(pair (not (fst n)) (snd n))");;

(*
simplify q2_defs "fst (neg_int (int _3))";;
simplify q2_defs "snd (neg_int (int _3))";;
simplify q2_defs "fst (neg_int (neg_int (int _3)))";;
simplify q2_defs "snd (neg_int (neg_int (int _3)))";;
*)

(*plus_int takes two encoded integers m and n and returns integer m+n*)
(*check the signs of m and n and based on that, determines whether to plus or minus and apply 
int or neg_int*)
let plus_int = ("plus_int",
	"/m./n.
	if (fst m) 
		(
			if (fst n) 
				(int (plus (snd m) (snd n)))
				(
					if (geq (snd m) (snd n)) 
					(int (minus (snd m) (snd n)))
					(neg_int (int (minus (snd n) (snd m))))
				)
		)
	(
		if (fst n) 
			(
				if (geq (snd m) (snd n)) 
				(neg_int (int (minus (snd m) (snd n))))
				(int (minus (snd n) (snd m)))
			)
		(neg_int (int (plus (snd m) (snd n))))
	)"
);;

(*
simplify q2_defs "fst (plus_int (int _3) (int _2))";;
simplify q2_defs "snd (plus_int (int _3) (int _2))";;
simplify q2_defs "fst (plus_int (int _3) (neg_int (int _2)))";;
simplify q2_defs "snd (plus_int (int _3) (neg_int (int _2)))";;
simplify q2_defs "fst (plus_int (neg_int (int _3)) (int _2))";;
simplify q2_defs "snd (plus_int (neg_int (int _3)) (int _2))";;
simplify q2_defs "fst (plus_int (neg_int (int _3)) (neg_int (int _2)))";;
simplify q2_defs "snd (plus_int (neg_int (int _3)) (neg_int (int _2)))";;
*)

(*times_int takes two encoded integers and returns integer mxn*)
let times_int = ("times_int", "/m./n.
	if (fst m) 
		(
			if (fst n) 
				(int (times (snd m) (snd n)))
				(neg_int (int (times (snd m) (snd n))))
		)
	(
		if (fst n) 
			(neg_int (int (times (snd m) (snd n))))
		(int (times (snd m) (snd n)))
	)"
);;

(*
simplify q2_defs "fst (times_int (int _3) (int _2))";;
simplify q2_defs "snd (times_int (int _3) (int _2))";;
simplify q2_defs "fst (times_int (int _3) (neg_int (int _2)))";;
simplify q2_defs "snd (times_int (int _3) (neg_int (int _2)))";;
simplify q2_defs "fst (times_int (neg_int (int _3)) (int _2))";;
simplify q2_defs "snd (times_int (neg_int (int _3)) (int _2))";;
simplify q2_defs "fst (times_int (neg_int (int _3)) (neg_int (int _2)))";;
simplify q2_defs "snd (times_int (neg_int (int _3)) (neg_int (int _2)))";;
*)

(* 
* Make sure all your definitions are added to this -- this is what I'll be testing 
*)

let q2_defs = default_defs @ [minus; geq; eq; pair; match_pair; fst; snd; update_fst; update_snd; int; neg_int; plus_int; times_int ]


(*************************************************************
 * Question 3
 *
 *************************************************************)

(*
* By default, all of these are implemented as identifier "not_implemented"
* 
* Just replace that with your own definition
*
*)

let empty = ("empty","/a./f.a");;

let cons = ("cons","/h./t./a./f.f h t");;

let match_list = ("match_list","/L./a./f. L a f ");;

let length = ("length","Y 
	(
		/length./L.
			match_list L _0 (/h./t. (plus _1 (length t)))
	)"
);;

(*sum has the same structure as length except instead of doing +1, we're summing the elements*)
let sum = ("sum","Y 
	(
		/sum./L.
			match_list L _0 (/h./t. (plus h (sum t)))
	)"
);;

(*append takes two encoded lists and returns the list made up of all elements of L1 followed by all 
elements of L2*)
let append = ("append","Y 
	(
		/append./L1./L2. match_list L1 L2 (/h./t. (cons h (append t L2)))
	)"
);;
	
(*map takes a function f and an encoded list L and returns the list obtained by applying function f 
to every element of L*)
let map = ("map","Y 
	(
		/map./f./L. match_list L empty (/h./t. cons (f h) (map f t))
	)"
);;

let q3_defs = default_defs @ [empty; cons; match_list; length; sum; append; map]
