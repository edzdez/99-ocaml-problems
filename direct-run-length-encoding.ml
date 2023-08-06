type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode =
    let make_tuple x cnt = if cnt = 1 then One x else Many (cnt, x) in
    let rec aux acc prev cnt = function
        | [] -> make_tuple prev cnt :: acc
        | (x :: xs) when x = prev -> aux acc prev (cnt + 1) xs
        | (x :: xs) -> aux (make_tuple prev cnt :: acc) x 1 xs
    in
    function
        | [] -> []
        | (x :: xs) -> List.rev (aux [] x 1 xs)

(*
# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]
*)
