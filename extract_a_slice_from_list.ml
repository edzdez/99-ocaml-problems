let slice xs a b =
    let rec aux1 i = function
    | [] -> []
    | x :: xs -> if i = a then aux2 [x] i xs else aux1 (i + 1) xs
    and aux2 acc i = function
    | [] -> List.rev acc
    | x :: xs -> if i = b then List.rev acc else aux2 (x :: acc) (i + 1) xs
    in
    aux1 0 xs

(*
# slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
- : string list = ["c"; "d"; "e"; "f"; "g"]
*)
