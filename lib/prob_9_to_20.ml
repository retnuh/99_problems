open Core_kernel
open Sexplib.Std

let p9_pack l =
  let rec aux acc cur = function
    | [] -> List.rev (cur :: acc)
    | x :: xs -> if (x = (List.hd_exn cur)) then aux acc (x :: cur) xs else aux (cur :: acc) [x] xs
  in match l with
  | [] -> []
  | x :: xs -> aux [] [x] xs

let%test _ = p9_pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] = 
             [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]


let p10_encode l = 
  List.map ~f:(fun x -> (List.length x, List.hd_exn x)) @@ p9_pack l

let%test _ = p10_encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = 
             [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]


type 'a rle = 
  | One of 'a 
  | Many of int * 'a [@@deriving sexp]

exception Programmer_Error of string


let p11_encode l = 
  let f = function
    | [] -> raise (Programmer_Error "empty rle list")
    | [x] -> One x
    | x :: _ as xs -> Many (List.length xs, x)
  in List.map ~f:f @@ p9_pack l

let%test _ = p11_encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = 
             [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]

let%expect_test _ =
  let ans = p11_encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] 
  and ll = sexp_of_list (sexp_of_rle sexp_of_string) in
  let sexp = ll ans |> Sexp.to_string in
  print_endline sexp;
  [%expect{|
    ((Many 4 a)(One b)(Many 2 c)(Many 2 a)(One d)(Many 4 e))
   |}] 


let p12_decode = 
  let rec repeat c x acc = match c with
    | 0 -> acc
    | n -> repeat (n - 1) x (x::acc)
  in let rec aux acc = function
      | [] -> List.rev acc
      | One x :: xs -> aux (x::acc) xs
      | Many (count, x) :: xs -> aux (repeat count x acc) xs
  in aux []

let%test _ = p12_decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] =
             ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]


let p13_encode = 
  let r c x = 
    if c = 1 then One x else Many (c, x)
  in let rec aux acc count = function
      | [] -> List.rev acc
      | [x] -> List.rev ((r (count + 1) x) :: acc)
      | a :: (b :: _ as rest) -> 
        if a = b then aux acc (count + 1) rest
        else  aux ((r (count + 1) a) :: acc) 0 rest
  in aux [] 0

let%test _ = p13_encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
             [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
              Many (4, "e")]

let%test _ = (p13_encode ["a"], p13_encode ["a"; "a"], p13_encode ["a"; "b"]) =
             ([ One "a"], [Many (2, "a")], [One "a"; One "b"])

let p14_duplicate =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> aux (x :: x :: acc) xs
  in aux []

let %test _ = p14_duplicate ["a";"b";"c";"c";"d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]

let p15_replicate k l =
  let rec repeat c x acc = match c with
    | 0 -> acc
    | n -> repeat (n - 1) x (x::acc)
  in List.rev @@ List.fold ~init:[] ~f:(fun acc x -> repeat k x acc) l

let%test _ = p15_replicate 3 ["a";"b";"c"]  = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]

let%expect_test _ =
  let ans = p15_replicate 3 ["a"; "b"; "c"] in
  print_endline (String.concat ~sep:" " ans);
  [%expect{|
    a a a b b b c c c
   |}] 

let p16_drop l k =
  let rec aux c acc = function 
    | [] -> List.rev acc
    | x:: xs -> 
      if c = 1 then aux k acc xs
      else aux (c - 1) (x::acc) xs
  in aux k [] l

let%test _ = p16_drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]

let p17_split l k =
  let rec aux c acc = function
    | [] -> (List.rev acc, [])
    | (x :: xs as all) -> if c = 0 then (List.rev acc, all) else (aux (c -1) (x::acc) xs) 
  in aux k [] l

let%test _ = p17_split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 =
             (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])

let%test _ = p17_split ["a";"b";"c";"d"] 5 = (["a"; "b"; "c"; "d"], [])

let p18_slice l i k = 
  List.take (List.drop l i) (k - i + 1)

let%test _ = p18_slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 = ["c"; "d"; "e"; "f"; "g"]

let p19_rotate l n =
  if n = 0 then l
  else let len = List.length l in
    let n = (n mod len + len) mod len in
    let (a, b) = p17_split l n 
    in b @ a


let%test _ = p19_rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 11 = 
             ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]

let%test _ = p19_rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-10) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]

let p20_remove_at k l =
  (* let rec splat acc = function
     | [] -> acc
     | x :: xs -> splat (x::acc) xs 
     in *)
  let (front, back) = p17_split l k in match back with 
  | [] -> front
  | [_] -> front
  | _ :: xs -> front @ xs


let%test _ = p20_remove_at 1 ["a";"b";"c";"d"] = ["a"; "c"; "d"]

