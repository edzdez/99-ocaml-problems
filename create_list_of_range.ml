let rec range a b =
    if a = b then [b]
    else a :: range (a + 1) b

(*
# range 4 9;;
- : int list = [4; 5; 6; 7; 8; 9]
*)
