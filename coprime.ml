let rec gcd a b =
    let r = a mod b in
    if r = 0 then b
    else gcd b r

let coprime a b = gcd a b = 1

(*
# coprime 13 27;;
- : bool = true
# not (coprime 20536 7826);;
- : bool = true
*)
