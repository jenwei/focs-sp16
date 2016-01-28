let rec listMax (xs) = 
  match xs with
    | [] -> failwith "empty"
    | [h] -> h
    | h :: rest -> max h (listMax (rest))
;;

listMax ([1;2;5;3]);; (*5*)

let rec zip (l1,l2) = 
  (*given two lists of equal size, merge the lists together by "zipping" them where [a1;a2] and [b1;b2] returns [a1;b1;a2;b2] *)
  match l1 with
    | [] -> []
    | h1 :: rest1 -> 
        match l2 with 
          | [] -> []
          | h2 :: rest2 -> h1 :: h2 :: zip(rest1,rest2)
;;

zip ([1;3;5],[2;4;6]);;


let compress (xs) =
  (*removes consecutive duplicates i.e. [a;a;a;b;b;b;c;c;a;a;a] -> [a;b;c;a]*)
  match xs with
    | [] -> []
    | [x] -> [x]
    | [x;y] -> if x = y then [x] else [x;y]
    | x1::x2::rest -> if x1 = x2 then compress (x2::rest) else x1::compress(x2::rest)
;;

compress ([1;1;2;3;4;4;4;4;3;2;2;2;1;1]);;

