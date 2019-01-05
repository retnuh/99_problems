open Core_kernel

let p21_insert_at item k = 
  let rec aux c acc = function 
    | [] -> List.rev (item :: acc)
    | x::xs as rest -> if c = 0 then List.rev_append acc (item::rest)
      else aux (c-1) (x::acc) xs
  in aux k []

let%test _ = p21_insert_at "alfa" 1 ["a";"b";"c";"d"] = ["a"; "alfa"; "b"; "c"; "d"]
let%test _ = p21_insert_at "alfa" 3 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "alfa"; "d"]
let%test _ = p21_insert_at "alfa" 4 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "d"; "alfa"]

let p22_range start stop =
  let (l, f) = if start <= stop then stop-start+1, (+) else start-stop+1, (-) in
  let x = List.init l ~f:(fun x -> f start x ) in
  (* (x) |> List.map ~f:Int.to_string |> String.concat ~sep:" " |> print_endline; *)
  x

let%test _ = p22_range 4 9 = [4; 5; 6; 7; 8; 9]
let%test _ = p22_range 9 4 = [9; 8; 7; 6; 5; 4]