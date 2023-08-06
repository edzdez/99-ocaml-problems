let insert_at elem pos xs =
    let rec aux acc i = function
    | [] -> [elem]
    | x :: xs as l -> if i = pos then List.rev (elem :: acc) @ l else aux (x :: acc) (i + 1) xs
    in
    aux [] 0 xs

(*
# insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
- : string list = ["a"; "alfa"; "b"; "c"; "d"]
*)
