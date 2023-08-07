(*
Sorting a list of lists according to length of sublists.

We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
*)
let split xs n =
    let rec aux acc i = function
    | [] -> (List.rev acc, [])
    | x :: xs ->
        if i = n then (List.rev (x :: acc), xs)
        else aux (x :: acc) (i + 1) xs
    in
    aux [] 1 xs

let rec merge (f : 'a -> 'a -> int) (l : 'a list) (r : 'a list) =
    match l, r with
    | [], r -> r
    | l ,[] -> l
    | (l :: ls), (r :: rs) ->
        if (f l r) <= 0 then l :: merge f ls (r :: rs)
        else r :: merge f (l :: ls) rs

let rec sort (f : 'a -> 'a -> int) (lst : 'a list) =
    let len = List.length lst in
    if len = 1 then lst
    else
        let (l, r) = split lst (len / 2) in
        merge f (sort f l) (sort f r)

(* I'm going to do these without List.sort *)
let length_sort (lst : 'a list list) =
    sort (fun a b -> List.length a - List.length b) lst

let frequency_sort (lst : 'a list list) =
    let lengths = List.map List.length lst in
    let frequency i = List.filter (fun a -> a = i) lengths |> List.length in
    sort (fun a b -> (frequency (List.length a)) - (frequency (List.length b))) lst

(*
# length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
             ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
- : string list list =
[["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
 ["i"; "j"; "k"; "l"]]
# frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
                ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
- : string list list =
[["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
 ["d"; "e"]; ["m"; "n"]]
*)
