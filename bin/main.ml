open Lib.Problems

let () =
  let x = match p1_last ["a"; "b"; "c"] with
  | None -> "<nada>" 
  | Some x -> x
  in print_endline x;;