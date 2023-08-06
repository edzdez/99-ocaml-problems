let pack =
    let rec aux acc curr =
        match curr with
        | [] -> assert false
        | (x :: xs) -> function
            | [] -> curr :: acc
            | (y :: ys) when x = y -> aux acc (y :: x :: xs) ys
            | (y :: ys) -> aux ((x :: xs) :: acc) [y] ys
    in
    function
        | [] -> []
        | (x :: xs) -> List.rev (aux [] [x] xs)

(*
# pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]
*)
