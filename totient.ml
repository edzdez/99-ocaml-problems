(*
Euler's so-called totient function φ(m) is defined as the number of positive integers r (1 ≤ r < m) that are coprime to m. We let φ(1) = 1.

Find out what the value of φ(m) is if m is a prime number. Euler's totient function plays an important role in one of the most widely used public key cryptography methods (RSA). In this exercise you should use the most primitive method to calculate this function (there are smarter ways that we shall discuss later).
*)

let rec range a b =
    let rec aux a b =
        if a = b then []
        else a :: range (a + 1) b
    in
    if a > b then aux b a else aux a b

let rec gcd a b =
    let r = a mod b in
    if r = 0 then b
    else gcd b r

let coprime a b = gcd a b = 1

let phi n =
    if n = 1 then 1
    else
    range 1 n
    |> List.filter (fun a -> coprime a n)
    |> List.length

(*
# phi 10;;
- : int = 4
*)
