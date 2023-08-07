(*
Group the elements of a set into disjoint subsets

In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
Generalize the above function in a way that we can specify a list of group sizes and the function will return a list of groups.
*)

(* TODO: currently counts the same set multiple times e.g. [a, b] <> [b, a] *)

let rec range a b =
    let rec aux a b =
        if a = b then []
        else a :: range (a + 1) b
    in
    if a > b then aux b a else aux a b

let extract lst n =
    (List.nth lst n, List.filteri (fun i _ -> n <> i) lst)

let extract_first_n lst n =
    let rec aux acc n = function
    | [] -> (List.rev acc, [])
    | x :: xs as l -> if n = 0 then (List.rev acc, l) else aux (x :: acc) (n - 1) xs
    in
    aux [] n lst

let rec permutations = function
    | [] -> [[]]
    | [x] -> [[x]]
    | lst -> List.fold_left (fun acc h -> acc @ List.map (fun p -> h :: p) (permutations (List.filter (fun x -> h <> x) lst))) [] lst

let group lst parts =
    let rec aux lst = function
    | [] -> []
    | x :: xs ->
        let (first, rest) = extract_first_n lst x in
        first :: aux rest xs
    in
    let permutations = permutations lst in
    permutations |> List.map (fun permutation -> aux permutation parts)

(*
# group ["a"; "b"; "c"; "d"] [2; 1];;
- : string list list list =
[[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]
*)
