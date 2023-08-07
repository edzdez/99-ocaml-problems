let rec range a b =
    let rec aux a b =
        if a = b then [b]
        else a :: range (a + 1) b
    in
    if a > b then aux b a else aux a b

let is_prime i =
    if i < 2 then false
    else if i = 2 then true
    else List.filter (fun n -> i mod n = 0) (range 2 (int_of_float (sqrt (float_of_int i) +. 1.))) |> List.length = 0

let all_primes a b =
    range a b |> List.filter is_prime

(*
# List.length (all_primes 2 7920);;
- : int = 1000
*)
