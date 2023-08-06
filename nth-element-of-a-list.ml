let rec nth lst k =
    match lst, k with
    | [], _ -> None
    | (x :: _), 0 -> Some x
    | (_ :: xs), n -> nth xs (n - 1)

(*
# List.nth ["a"; "b"; "c"; "d"; "e"] 2;;
- : string = "c"
# List.nth ["a"] 2;;
Exception: Failure "nth".
*)
