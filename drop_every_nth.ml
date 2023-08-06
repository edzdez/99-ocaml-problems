let drop (lst : 'a list) (n : int) =
    let rec aux i = function
        | [] -> []
        | x :: xs -> if i = n then aux 1 xs else x :: aux (i + 1) xs
        in
    aux 1 lst

(*
# drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
*)
