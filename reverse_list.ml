(* Alex Fifer *)

(* Reverse a list.  *)
let rec reverse l =
     match l with
     | [] -> []
     | (h::t) -> (reverse t) @ [h] ;;
     
     
(*   Type:   'a list -> ('a list -> 'b) -> 'b   *)
let rec cont_reverse l f =
    match l with
    | [] -> (f [])
    | h::t -> cont_reverse t (fun x -> f([h] @ x)) ;;
    
    
