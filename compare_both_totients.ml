module Totient = struct
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
end

module TotientImproved = struct
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
end

let timeit f a =
    let start = Sys.time () in
    f a;
    let finish = Sys.time () in
    finish -. start

(*
timeit phi 10090
*)
