type bool_expr =
    Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec cartesian xs ys =
    match xs, ys with
    | [], [] -> []
    | x :: xs, y :: ys -> (x, y) :: cartesian xs ys
    | _ -> failwith "invalid sizes"

let rec make_values n =
    if n = 1 then [[true]; [false]]
    else List.map (fun a -> true :: a) (make_values (n - 1)) @ List.map (fun a -> false :: a) (make_values (n - 1))

let rec index_of x = function
    | [] -> failwith "not found"
    | h :: t -> if h = x then 0 else 1 + index_of x t

let rec eval expr vars vals =
    match expr with
    | Var x -> List.nth vals (index_of x vars)
    | Not e -> not (eval e vars vals)
    | And (l, r) -> (eval l vars vals) && (eval r vars vals)
    | Or (l, r) -> (eval l vars vals) || (eval r vars vals)

let table vars expr =
    let values = make_values (List.length vars) in
    List.map (fun v -> ((cartesian vars v), eval expr vars v)) values

(*
# table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")));;
- : ((string * bool) list * bool) list =
[([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
 ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]
*)
