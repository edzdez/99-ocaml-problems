let rec last_two = function
    | [] | [_] -> None
    | [x; y] -> Some (x, y)
    | _ :: xs -> last_two xs

(*
# last_two ["a"; "b"; "c"; "d"];;
- : (string * string) option = Some ("c", "d")
# last_two ["a"];;
- : (string * string) option = None
*)

