let rev lst =
    let rec aux acc = function
        | [] -> acc
        | (x :: xs) -> aux (x :: acc) xs
    in
    aux [] lst

(*
# rev ["a"; "b"; "c"];;
- : string list = ["c"; "b"; "a"]
*)
