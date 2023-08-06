let extract lst n =
    let rec aux acc i = function
    | [] -> raise (Invalid_argument "index out of range")
    | x :: xs -> if i = n then (x, List.rev acc @ xs) else aux (x :: acc) (i + 1) xs
    in
    aux [] 0 lst

let rand_select lst n =
    let lst_len = List.length lst in
    let rec aux acc lst i =
        if i = n then List.rev acc
        else
            let (x, xs) = extract lst (Random.int (lst_len - i)) in
            aux (x :: acc) xs (i + 1)
    in
    aux [] lst 0

(*
# rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
- : string list = ["g"; "d"; "a"]
*)
