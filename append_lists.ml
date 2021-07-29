(* Alex Fifer *)

(* various definitions of an append function.

let rec append l1 l2 =
     match l1 with
     | [] -> l2
     | (h::t) -> h :: append t l2

let rec append l1 =
    fun l2 ->
     match l1 with
     | [] -> l2
     | (h::t) -> h :: append t l2

let rec append l1 =
     match l1 with
     | [] -> fun l2 -> l2
     | (h::t) -> fun l2 -> h :: append t l2

*)

(* Recursive function to append one list to the tail of another.  *)
let rec append l =
  match l with
  | [] -> (fun l2 -> l2)
  | (h::t) ->
      let tail_appender = append t in
        (fun l2 -> h :: tail_appender l2) ;;

(* faster and better than "append"  *)
let rec cont_append l1 l2 c =
  match l1 with
  | [] -> (c l2)
  | h::t -> cont_append t l2 (fun x -> c (h :: l2)) ;;
        
(*
let rec cont_append l1 l2 f =   
  match l1 with    
  | [] -> (f l2)    
  | h::t -> cont_append l1 l2 (fun x -> f([h] @ x)) ;;
*)

cont_append [1; 2] [3; 4] (fun l -> l);;

