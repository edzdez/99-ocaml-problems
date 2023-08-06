(*
Generate a random permutation of the elements of a list.
*)

let extract lst n =
    (List.nth lst n, List.filteri (fun i _ -> n <> i) lst)

let permutation lst =
    let rec aux acc n xs =
        if n = 0 then acc
        else
            let (x, xs) = extract xs (Random.int n) in
            aux (x :: acc) (n - 1) xs
    in
    aux [] (List.length lst) lst

(*
# permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
- : string list = ["c"; "d"; "f"; "e"; "b"; "a"]
*)
