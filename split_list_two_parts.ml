let split xs n =
    let rec aux acc i = function
    | [] -> (List.rev acc, [])
    | x :: xs ->
        if i = n then (List.rev (x :: acc), xs)
        else aux (x :: acc) (i + 1) xs
    in
    aux [] 1 xs

(*
# split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
- : string list * string list =
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
# split ["a"; "b"; "c"; "d"] 5;;
- : string list * string list = (["a"; "b"; "c"; "d"], [])
*)
