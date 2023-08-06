type 'a node =
| One of 'a
| Many of 'a node list

(*
let rec flatten =
    let aux = function
        | One x -> [x]
        | Many xs -> flatten xs
    in
    function
    | [] -> []
    | (x :: xs) -> (aux x) @ (flatten xs)
*)

let rec flatten xs =
    let rec aux = function
        | One x -> [x]
        | Many xs -> flatten_helper [] xs
    and flatten_helper acc = function
        | [] -> acc
        | (x :: xs) -> (aux x) @ (flatten_helper [] xs)
    in
    flatten_helper [] xs

(*
# flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
- : string list = ["a"; "b"; "c"; "d"; "e"]
*)
