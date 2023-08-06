(*
Draw N different random numbers from the set 1..M.

The selected numbers shall be returned in a list.
*)

let rec lotto_select n m =
    if n = 0 then []
    else (Random.int m) + 1 :: lotto_select (n - 1) m

(*
# lotto_select 6 49;;
- : int list = [20; 28; 45; 16; 24; 38]
*)
