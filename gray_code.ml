(*
An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,

```
n = 1: C(1) = ['0', '1'].
n = 2: C(2) = ['00', '01', '11', '10'].
n = 3: C(3) = ['000', '001', '011', '010', '110', '111', '101', '100'].
```

Find out the construction rules and write a function with the following specification: gray n returns the n-bit Gray code.
*)

let gray n =
    let rec next i l =
        if i <> n then
            let l' = List.rev l in
            List.map (fun e -> "0" ^ e) l @ List.map (fun e -> "1" ^ e) l'
            |> next (i + 1)
        else l
    in
    next 1 ["0"; "1"]

(*
# gray 1;;
- : string list = ["0"; "1"]
# gray 2;;
- : string list = ["00"; "01"; "11"; "10"]
# gray 3;;
- : string list = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]
*)
