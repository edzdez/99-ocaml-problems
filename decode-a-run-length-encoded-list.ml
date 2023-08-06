type 'a rle =
  | One of 'a
  | Many of int * 'a

let repeat x n =
    let rec aux acc = function
        | 0 -> acc
        | n -> aux (x :: acc) (n - 1)
    in
    aux [] n

let decode xs =
    let rec aux acc = function
        | [] -> acc
        | One x :: xs -> aux ([x] @ acc) xs
        | Many (n, x) :: xs -> aux (repeat x n @ acc) xs
    in
    List.rev (aux [] xs)

(*
# decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
*)
