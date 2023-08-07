let factors n =
    let rec aux acc d n =
        if n = 1 then List.rev acc
        else if n mod d = 0 then aux (d :: acc) d (n / d)
        else aux acc (d + 1) n
    in
    aux [] 2 n

(*
# factors 315;;
- : int list = [3; 3; 5; 7]
*)
