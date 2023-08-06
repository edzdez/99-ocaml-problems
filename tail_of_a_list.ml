let rec last lst =
    match lst with
    | [] -> None
    | x :: [] -> Some x
    | _ :: xs -> last xs

(*
# last ["a" ; "b" ; "c" ; "d"];;
- : string option = Some "d"
# last [];;
- : 'a option = None
*)
