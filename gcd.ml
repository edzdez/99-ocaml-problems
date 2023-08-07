let rec gcd a b =
    let r = a mod b in
    if r = 0 then b
    else gcd b r

(*
# gcd 13 27;;
- : int = 1
# gcd 20536 7826;;
- : int = 2
*)
