(*
Generate the combinations of K distinct objects chosen from the N elements of a list.

In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
*)

let rec range a b =
    let rec aux a b =
        if a = b then []
        else a :: range (a + 1) b
    in
    if a > b then aux b a else aux a b

(* definitely not the most efficient way to do this *)
let extract n lst =
    let b = List.length lst in
    let rec aux curr a i =
        if i = 0 then [List.rev curr]
        else List.map (fun j -> aux (j :: curr) (j + 1) (i - 1)) (range a b)
             |> List.fold_left (fun acc x -> x @ acc) []
    in
    let indices = aux [] 0 n |> List.rev in
    List.map (fun indices -> List.map (fun i -> List.nth lst i) indices) indices

(*
# extract 2 ["a"; "b"; "c"; "d"];;
- : string list list =
[["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
*)
