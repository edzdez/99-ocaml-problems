let compress =
    let rec aux acc prev = function
        | [] -> acc
        | (x :: xs) when x = prev -> aux acc prev xs
        | (x :: xs) -> aux (x :: acc) x xs
    in
    function
        | [] -> []
        | (x :: xs) -> List.rev (aux [x] x xs)

(*
# compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
*)
