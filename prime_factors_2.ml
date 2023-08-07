let factors n =
    let rec aux acc (curr, count) d n =
        if n = 1 then List.rev ((curr, count) :: acc)
        else if n mod d = 0 then
            if curr = d then aux acc (curr, count + 1) d (n / d)
            else if count <> 0 then aux ((curr, count) :: acc) (d, 1) d (n / d)
            else aux acc (d, 1) d (n / d)
        else aux acc (curr, count) (d + 1) n
    in
    aux [] (2, 0) 2 n

(*
# factors 315;;
- : (int * int) list = [(3, 2); (5, 1); (7, 1)]
*)
