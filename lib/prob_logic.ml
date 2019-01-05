open Core_kernel
(* exception BadVariable of string *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

let p46_eval2 a val_a _ val_b = 
  let rec eval = function
    | Var x -> if x = a then val_a else val_b (* cheating here not checking if it's = b *)
    | Not ex -> not (eval ex)
    | And (e1, e2) -> (eval e1) && (eval e2)
    | Or (e1, e2) -> (eval e1) || (eval e2)
  in eval

let p47_table2 a b expr = 
  let f val_a val_b =
    let ans = p46_eval2 a val_a b val_b expr in
    (* List.iter ~f:(fun b -> print_string (string_of_bool b); print_string " ") [val_a; val_b; ans]; print_endline ""; *)
    (val_a, val_b, ans) 
  in List.([true; false] >>= fun a ->
           [true; false] >>= fun b ->
           [f a b])

let%test _ = p47_table2 "a" "b" (And(Var "a", Or(Var "a", Var "b"))) =
             [(true, true, true); (true, false, true); (false, true, false);
              (false, false, false)]

type name_val_alist = (string, bool) List.Assoc.t

let rec p48_eval (vals: name_val_alist) = function
  | Var x -> List.Assoc.find_exn vals ~equal:String.equal x 
  | Not ex -> not (p48_eval vals ex)
  | And (e1, e2) -> (p48_eval vals e1) && (p48_eval vals e2)
  | Or (e1, e2) -> (p48_eval vals e1) || (p48_eval vals e2)

let flat_map = List.(>>=)

let p48_table (names: string list) expr =
  let add_name_vals existing name = 
    flat_map [true; false] (fun b -> List.map existing ~f:(fun alist -> List.Assoc.add alist ~equal:String.equal name b))
  in let rec aux = function
      | [] -> ([] :: [])
      | name :: rest -> add_name_vals (aux rest) name

  in let all_vals = aux names 
  in List.map ~f:(fun vals -> (vals, p48_eval vals expr)) all_vals

let%test _ = p48_table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b"))) =
             [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
              ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]

let%test _ = let a = Var "a" and b = Var "b" and c = Var "c" in
  p48_table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c)))) =
  [([("a", true); ("b", true); ("c", true)], true);
   ([("a", true); ("b", true); ("c", false)], true);
   ([("a", true); ("b", false); ("c", true)], true);
   ([("a", true); ("b", false); ("c", false)], false);
   ([("a", false); ("b", true); ("c", true)], false);
   ([("a", false); ("b", true); ("c", false)], false);
   ([("a", false); ("b", false); ("c", true)], false);
   ([("a", false); ("b", false); ("c", false)], false)]

type p49_direction = Forwards | Backwards

let p49_list_for_direction  = function
  | Forwards -> ["0"; "1"]
  | Backwards -> ["1"; "0"]

let p49_other_direction = function
  | Forwards -> Backwards
  | Backwards -> Forwards

let p49_gray n =
  if n < 1 then failwith "n must be >= 1" else
    let rec combine acc dir = function
      | [] -> List.rev acc
      | item :: rest -> 
        let p = p49_list_for_direction dir in 
        combine ((List.fold p ~f:(fun a x -> (item^x) :: a)) ~init:acc) (p49_other_direction dir) rest
    in let rec aux = function
        | 1 -> ["0"; "1"]
        | x -> let prev = aux (x-1) in 
          combine [] Forwards prev
    in aux n

let%test _ = p49_gray 1 = ["0"; "1"]
let%test _ = p49_gray 2 = ["00"; "01"; "11"; "10"]
let%test _ = p49_gray 3 = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]

let p49_solution_gray n =
  let rec gray_next_level k l =
    if k<n then
      (* This is the core part of the Gray code construction.
       * first_half is reversed and has a "0" attached to every element.
       * Second part is reversed (it must be reversed for correct gray code).
       * Every element has "1" attached to the front.*)
      let (first_half,second_half) =
        List.fold ~f:(fun (acc1,acc2) x ->
            (("0"^x)::acc1, ("1"^x)::acc2 )) ~init:([],[]) l
      in
      (* List.rev_append turns first_half around and attaches it to second_half.
       * The result is the modified first_half in correct order attached to
       * the second_half modified in reversed order.*)
      gray_next_level (k+1) (List.rev_append first_half second_half)
    else l
  in
  gray_next_level 1 ["0"; "1"];;

let%test _ = p49_gray 4 = p49_solution_gray 4


type p50_huffman_tree = 
  | Node of { freq: int; left: p50_huffman_tree; right: p50_huffman_tree }
  | Leaf of { freq: int; char: string } [@@deriving sexp]

let p50_ht_freq = function
  | Node {freq; _} -> freq
  | Leaf {freq; _} -> freq 

let p50_ht_compare a b = Int.compare (p50_ht_freq a) (p50_ht_freq b)

(* Actually mostly unnecessary, nodes come out of heap in order! *)
let p50_ht_merge a b = let fa, fb = (p50_ht_freq a), (p50_ht_freq b) in
  let node = Node {freq = fa+fb; left = a; right = b} in 
  (* [a; b; node] |> List.sexp_of_t sexp_of_p50_huffman_tree |> Sexp.to_string |> print_endline; *)
  node

let p50_huffman freq_list =
  let rec to_tree heap = match Fheap.length heap with 
    | 0 -> failwith "too far!"
    | 1 -> Fheap.top_exn heap
    | _ -> 
      let a, heap' = Fheap.pop_exn heap in 
      let b, heap'' = Fheap.pop_exn heap' in 
      to_tree @@ Fheap.add heap'' @@ p50_ht_merge a b 
  in let rec encode acc current_code = function
      | Leaf { char; _} -> (char, current_code) :: acc
      | Node { left; right; _} -> 
        let acc' = encode acc (current_code^"1") right in
        encode acc' (current_code^"0") left
  in 
  List.map ~f:(fun (char, freq) -> Leaf {freq; char} ) freq_list 
  |> List.fold ~init:(Fheap.create ~cmp:p50_ht_compare) ~f:Fheap.add 
  |> to_tree |> encode [] "" 

let p50_fs = [ ("a", 45); ("b", 13); ("c", 12); ("d", 16);
               ("e", 9); ("f", 5) ];;

let%test _ = 
  let ans = p50_huffman p50_fs in 
  List.iter ans ~f:(fun x ->
      [%sexp_of: string * string ] x
      |> Sexp.to_string
      |> print_endline);
  ans = [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101");
         ("d", "111")]

let%test _ = p50_huffman ["a", 10;  "b", 15;  "c", 30;  "d", 16;  "e", 29] = 
             [("d", "00"); ("a", "010"); ("b", "011"); ("e", "10"); ("c", "11")]

