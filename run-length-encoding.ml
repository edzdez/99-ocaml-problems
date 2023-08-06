let encode =
    let rec aux acc (n, a) = function
        | [] -> (n, a) :: acc
        | (x :: xs) when a = x -> aux acc (n + 1, a) xs
        | (x :: xs) -> aux ((n, a) :: acc) (1, x) xs
    in
    function
        | [] -> []
        | (x :: xs) -> List.rev (aux [] (1, x) xs)

(*
# encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
*)
