open Core
open Poly
(* open Sexplib.Std *)

(* let (=) = Pervasives.(=) *)


let%test _ = Some "a" = Some "a"

(* last : 'a list -> 'a option *)
let rec p1_last = function
  | [] -> None
  | [x] -> Some x
  | _ :: rest -> p1_last rest

let%test _ = p1_last [ "a" ; "b" ; "c" ; "d" ] = Some "d"

let%test _ = p1_last [] = None 


let rec p2_last_two = function
  | [] -> None  
  | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: rest -> p2_last_two rest

let%test _ = p2_last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d")
let%test _ = p2_last_two [ "a" ] = None

let rec p3_at k = function 
  | [] -> None
  | x :: _ when k = 1 -> Some x
  | _ :: xs -> p3_at (k -1) xs

let%test _ = p3_at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c"
let%test _ = p3_at 3 [ "a" ] = None

let p4_length =
  let rec length c = function
    | [] -> c
    | _ :: xs -> length (c+1) xs
  in length 0

let%test _ = p4_length [ "a" ; "b" ; "c"]= 3
let%test _ = p4_length [] = 0

let p5_rev =
  let rec rev acc = function
    | [] -> acc
    | x :: xs -> rev (x :: acc) xs
  in rev []

let%test _ = p5_rev ["a" ; "b" ; "c"] = ["c"; "b"; "a"]

let p6_is_palindrome l =
  l = (p5_rev l)

let%test _ = p6_is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true
let%test _ = not (p6_is_palindrome [ "a" ; "b" ]) = true

(* There is no nested list type in OCaml, so we need to define one
    first. A node of a nested list is either an element, or a list of
    nodes. *)
type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let p7_flatten l = 
  let rec aux acc = function
    | [] -> acc
    | One x :: xs -> aux (x :: acc) xs
    | Many ys :: xs -> aux (aux acc ys) xs
  in List.rev @@ aux [] l

let%test _ = p7_flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a"; "b"; "c"; "d"; "e"]

let%expect_test _ =
  let ans = p7_flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] in
  print_endline (String.concat ~sep:" " ans );
  [%expect{|
    a b c d e
  |}]

let p8_compress =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> 
      ( 
        (* print_endline (String.concat " " (x :: acc)); *)
        let acc' = match acc with
          | [] -> [x]
          | y :: _ -> if x = y then acc else x :: acc 
        in aux acc' xs)
  in aux [] 

let%test _ = p8_compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a"; "b"; "c"; "a"; "d"; "e"]

let%expect_test _ =
  let ans = p8_compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] in
  print_endline (String.concat ~sep:" " ans);
  [%expect{|
    a b c a d e
  |}]

