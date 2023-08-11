(*
Group the elements of a set into disjoint subsets

In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
Generalize the above function in a way that we can specify a list of group sizes and the function will return a list of groups.
*)

let rec range a b =
    let rec aux a b =
        if a = b then []
        else a :: range (a + 1) b
    in
    if a > b then aux b a else aux a b

(* definitely not the most efficient way to do this *)
let combinations n lst =
    let b = List.length lst in
    let rec aux curr a i =
        if i = 0 then [List.rev curr]
        else List.map (fun j -> aux (j :: curr) (j + 1) (i - 1)) (range a b)
             |> List.fold_left (fun acc x -> x @ acc) []
    in
    let indices = aux [] 0 n |> List.rev in
    List.map (fun indices -> List.map (fun i -> List.nth lst i) indices) indices

let rec remove_group lst = function
    | [] -> lst
    | x :: xs -> remove_group (lst
        |> List.filter (fun y -> x <> y))
        xs

let group lst parts =
    let rec aux (lst : int list)  = function
    | ([] : int list) -> failwith "uh oh"
    | [x] -> combinations x lst |> List.map (fun group -> [group])
    | (x :: xs) -> combinations x lst
        |> List.map (fun group ->
            let remaining = remove_group lst group in
            let groups = aux remaining xs in
            List.map (fun grp -> group :: grp) groups)
        |> List.flatten
    in
    let groups_with_indices = aux (range 0 (List.length lst)) parts in
    groups_with_indices
    |> List.map (fun groups ->
        groups
        |> List.map (fun group ->
            group
            |> List.map (fun x -> List.nth lst x)))

(*
# group ["a"; "b"; "c"; "d"] [2; 1];;
- : string list list list =
[[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]
*)
