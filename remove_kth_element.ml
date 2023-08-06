let remove_at k l =
    let rec aux acc i = function
    | [] -> List.rev acc
    | x :: xs -> if i = k then acc @ xs else aux (x :: acc) (i + 1) xs
    in
    aux [] 0 l

(*
# remove_at 1 ["a"; "b"; "c"; "d"];;
- : string list = ["a"; "c"; "d"]
*)
