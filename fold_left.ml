(* Alex Fifer *)

(* foldleft2 calls the function f on the elements in lists l1 and l2 
   and stores the accumulated list in the list u.   *)
let rec foldleft2 f l1 l2 u =
  match l1 with
  | [] -> u
  | (h1::t1) -> match l2 with
              | [] -> u
              | (h2::t2) -> foldleft2 f t1 t2 (f h1 h2 u) ;;
  
