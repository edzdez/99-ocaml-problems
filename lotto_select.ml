(*
Draw N __different__ random numbers from the set 1..M.

The selected numbers shall be returned in a list.
*)

let rec range a b =
    let rec aux a b =
        if a = b then [b]
        else a :: range (a + 1) b
    in
    if a > b then aux b a else aux a b

let extract lst n =
    (List.nth lst n, List.filteri (fun i _ -> n <> i) lst)

let rand_select lst n =
    let lst_len = List.length lst in
    let rec aux acc lst i =
        if i = n then List.rev acc
        else
            let (x, xs) = extract lst (Random.int (lst_len - i)) in
            aux (x :: acc) xs (i + 1)
    in
    aux [] lst 0


let rec lotto_select n m =
    rand_select (range 1 m) n

(*
# lotto_select 6 49;;
- : int list = [20; 28; 45; 16; 24; 38]
*)
