module Heap = struct
  type 'a t = { mutable data : 'a array; mutable len: int; comp : 'a -> 'a -> bool}

  let parent (i : int) = i / 2

  let left (i : int) = i * 2

  let right (i : int) = i * 2 + 1

  let valid_idx (h : 'a t) (i : int) = i >= 1 && i <= h.len

  let find_min (h : 'a t) (a : int) (b : int) = if h.comp h.data.(a) h.data.(b) then a else b

  let swap (h : 'a t) (a : int) (b : int) =
    let tmp = h.data.(a) in
    h.data.(a) <- h.data.(b);
    h.data.(b) <- tmp

  let heapify_down (h : 'a t) (i : int) =
    let rec aux (i : int) =
      let l = left i in
      let r = right i in
      if not (valid_idx h l) then ()
      else if valid_idx h r then (let min = find_min h l r in
                                  if h.comp h.data.(min) h.data.(i) then (swap h i min; aux min) else ())
      else if h.comp h.data.(l) h.data.(i) then (swap h i l; aux l) else ()
    in aux i

  let heapify_up (h : 'a t) (i : int) =
    let rec aux (i : int) =
      let p = parent i in
      if h.comp h.data.(i) h.data.(p) then (swap h p i; aux p) else ()
    in aux i

  let build_heap (h : 'a t) =
    let rec aux = function
      | 0 -> ()
      | i -> heapify_down h i; aux (i - 1) in
    aux h.len

  let make (data : 'a list) (comp : 'a -> 'a -> bool)=
    let h = { data = Array.of_list ((List.hd data) :: data); len = List.length data; comp} in
    build_heap h;
    h

  let size (h : 'a t) = h.len

  let extract_min (h : 'a t) =
    if h.len = 0 then invalid_arg "empty heap"
    else (let min = h.data.(1) in
          h.data.(1) <- h.data.(h.len);
          h.len <- h.len - 1;
          heapify_down h 1;
          min)

  let push (h : 'a t) (n : 'a) =
    let cap = Array.length h.data in
    (if h.len + 1 >= cap then
      h.data <- Array.append h.data h.data
    else ());
    h.len <- h.len + 1;
    h.data.(h.len) <- n;
    heapify_up h (h.len);
end

type node =
  | Nil
  | Node of { data : string; freq : int; left : node; right : node }

let fs = [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)]

let make_leaf ((c, f) :  string * int) = Node { data = c; freq = f; left = Nil; right = Nil }

let make_internal (left : node) (right : node) =
  match (left, right) with
  | (Node l, Node r) -> Node { data = ""; freq = l.freq + r.freq; left; right}
  | _ -> failwith "unreachable"

let huffman_tree fs =
  let comp (a : node) (b : node) =
    match (a, b) with
    | (Node a, Node b) -> a.freq < b.freq
    | _ -> failwith "unreachable"
  in
  let pq = Heap.make (List.map make_leaf fs) comp in
  let rec aux pq = if Heap.size pq = 1
    then Heap.extract_min pq
    else (let a = Heap.extract_min pq in let b = Heap.extract_min pq in Heap.push pq (make_internal a b); aux pq)
  in aux pq

let prefixes tree =
  let rec aux path = function
    | Nil -> failwith "unreachable"
    | Node { data; freq = _; left = Nil; right = Nil } -> [(data, path)]
    | Node { data = _; freq = _; left; right } -> (aux (path ^ "0") left) @ (aux (path ^ "1") right)
  in aux "" tree

let huffman (fs : (string * int) list) =
  let tree = huffman_tree fs in
  prefixes tree

(*
# huffman fs;;
- : (string * string) list =
[("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101");
 ("d", "111")]
# huffman [("a", 10); ("b", 15); ("c", 30); ("d", 16); ("e", 29)];;
- : (string * string) list =
[("d", "00"); ("a", "010"); ("b", "011"); ("e", "10"); ("c", "11")]
*)
