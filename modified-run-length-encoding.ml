type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode =
    let rec aux acc curr = function
        | [] -> curr :: acc
        | (x :: xs) -> match curr with
            | One a -> if a = x
                then aux acc (Many (2, x)) xs
                else aux (curr :: acc) (One x) xs
            | Many (n, a) -> if a = x
                then aux acc (Many (n + 1, x)) xs
                else aux (curr :: acc) (One x) xs
    in
    function
    | [] -> []
    | (x :: xs) -> List.rev (aux [] (One x) xs)

(*
# encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]
*)
