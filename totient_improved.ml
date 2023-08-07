(*
See problem "Calculate Euler's totient function φ(m)" for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of the previous problem then the function phi(m) can be efficiently calculated as follows: Let [(p1, m1); (p2, m2); (p3, m3); ...] be the list of prime factors (and their multiplicities) of a given number m. Then φ(m) can be calculated with the following formula:

φ(m) = (p1 - 1) × p1m1 - 1 × (p2 - 1) × p2m2 - 1 × (p3 - 1) × p3m3 - 1 × ⋯
*)

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

let rec pow n = function
    | 0 -> 1
    | m -> n * pow n (m - 1)

let phi_improved n =
    let prime_factors = factors n in
    prime_factors
    |> List.map (fun (p, m) -> (p - 1) * pow p (m - 1))
    |> List.fold_left (fun acc i -> acc * i) 1

(*
# phi_improved 10;;
- : int = 4
# phi_improved 13;;
- : int = 12
*)
