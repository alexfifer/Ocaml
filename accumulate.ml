(* Alex Fifer  *)

(* accumulate a list where an input function has been called on each element. 
   Pass an empty list as the last arg or pass a list with elements, who cares? *)

(* Type:    ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b  
   Preconditions: f is a function that takes 2 args
   Postcondition: *)
let rec accumulate f lst u =
   match lst with
      | [] -> u
      | (h::t) -> accumulate f t (f h u) ;;


      
(* Helper function for "union" function below.  *)
(*  "is_in" takes a list and an element to check for as arguments
    It has type:  'a list -> 'a -> bool         *)
    
let rec is_in l k =
  match l with
    [] -> false
  | (h :: t) -> if h = k then true
                else is_in t k ;;

(* union has type:  'a list -> 'a list -> 'a list  *)

let union l1 l2 = accumulate (fun h g -> if is_in g h then g else (h :: g)) l1 l2 ;;

(* reverse has type: 'a list -> 'a list
   Preconditions: f is a function that takes 2 args
   Postcondition: *)
let reverse l1 = accumulate (fun x y -> x :: y) l1 [] ;;

