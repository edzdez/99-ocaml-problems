(*
Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
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

let goldbach_list a b =
    range a b
    |> List.filter (fun i -> i mod 2 = 0)
    |> List.map (fun i -> (i, goldbach i))


(*
# goldbach_list 9 20;;
- : (int * (int * int)) list =
[(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
 (20, (3, 17))]
*)

(*
  # goldbach_list 3 3000 |> List.filter (fun (_, (a, _)) -> a > 50) |> List.length;;
  answer to part 2 is 10
*)
