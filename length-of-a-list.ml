let length lst =
    let rec helper lst n =
        match lst with
        | [] -> n
        | (_ :: xs) -> helper xs (n + 1)
    in
    helper lst 0

(*
# length ["a"; "b"; "c"];;
- : int = 3
# length [];;
- : int = 0
*)
