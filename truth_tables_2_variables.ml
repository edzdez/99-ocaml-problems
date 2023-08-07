type bool_expr =
    Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

(*
# And (Or (Var "a", Var "b"), And (Var "a", Var "b"));;
- : bool_expr = And (Or (Var "a", Var "b"), And (Var "a", Var "b"))
*)

(*
# table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")));;
- : (bool * bool * bool) list =
[(true, true, true); (true, false, true); (false, true, false);
 (false, false, false)]
*)

let rec eval expr (a, a_val) (b, b_val) =
    match expr with
    | Var e -> if e = a then a_val else if e = b then b_val else failwith "invalid variable"
    | Not e -> not (eval e (a, a_val) (b, b_val))
    | And (l, r) -> (eval l (a, a_val) (b, b_val)) && (eval r (a, a_val) (b, b_val))
    | Or (l, r) -> (eval l (a, a_val) (b, b_val)) || (eval r (a, a_val) (b, b_val))

let table2 a b expr =
    [0; 1; 2; 3] |> List.map (fun i ->
        let (a_val, b_val) = (i < 2, i mod 2 = 0) in
        (a_val, b_val, eval expr (a, a_val) (b, b_val)))
