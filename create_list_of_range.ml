(* If first argument is greater than second, produce a list in decreasing order. *)

let rec range a b =
    let rec aux a b =
        if a = b then [b]
        else a :: range (a + 1) b
    in
    if a > b then aux b a else aux a b

(*
# range 4 9;;
- : int list = [4; 5; 6; 7; 8; 9]
*)
