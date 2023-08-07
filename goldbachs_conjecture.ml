(*
Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers. Write a function to find the two prime numbers that sum up to a given even integer.
*)

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

let goldbach n =
    let rec aux i =
        if i = n then raise (Failure "oh no!")
        else if is_prime i && is_prime (n - i) then
            (i, n - i)
        else aux (i + 1)
    in
    aux 2

(*
# goldbach 28;;
- : int * int = (5, 23)
*)
