(* from split_list_two_parts *)
let split xs n =
    let rec aux acc i = function
    | [] -> (List.rev acc, [])
    | x :: xs ->
        if i = n then (List.rev (x :: acc), xs)
        else aux (x :: acc) (i + 1) xs
    in
    aux [] 1 xs

let rotate l n =
    let (a, b) = split l n in
    b @ a


(*
# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
- : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
*)
