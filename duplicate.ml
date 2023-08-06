let rec duplicate xs =
    let rec aux acc = function
        | [] -> acc
        | (x :: xs) -> aux (x :: x :: acc) xs
    in
    List.rev (aux [] xs)

(*
# duplicate ["a"; "b"; "c"; "c"; "d"];;
- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
*)
