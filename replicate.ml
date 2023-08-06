let repeat x n =
    let rec aux acc = function
        | 0 -> acc
        | n -> aux (x :: acc) (n - 1)
    in
    aux [] n

let replicate xs n =
    let rec aux acc = function
        | [] -> acc
        | (x :: xs) -> aux (repeat x n @ acc) xs
    in
    List.rev (aux [] xs)

(*
replicate ["a"; "b"; "c"] 3;;
- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
*)
