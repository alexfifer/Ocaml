(* Alex Fifer  *)

(* "reduce" takes a function, a list, and an environment list as args. 
   Type: 
   precondition: the function must take the list elements' type.   *)


let rec reduce f lst u =
   match lst with
     | [] -> u
     | (h::t) -> f h (reduce f t u) ;;


(* unzip has type: ('a * 'b) list -> 'a list * 'b list  *)
(* type: ('a * 'b) list -> 'a list * 'b list
   precondition: reduce must be defined.   *)
   
let unzip l = reduce (fun (h1, h2) -> fun (ul1, ul2) -> (h1::ul1, h2::ul2)) l ([] ,[]) ;;


(* An "append" function that uses "reduce"  *)
(* Type:     'a list -> 'a list -> 'a list 
   Preconditions: 
   Postcondition:  *)
let append l1 l2 = reduce (fun x y -> x :: y) l1 l2 ;;


(* A "filter" function that takes a function and a list *)
(* filter has type: ('a -> bool) -> 'a list -> 'a list
   Preconditions: f is a bool function
   Postcondition: *)
let filter f l1 = reduce (fun x y -> if f x then x :: y else y) l1 [] ;;


(*  Sample calls to filter  *)
filter (fun x -> (x mod 2 = 0)) [1;2;3;4;5;6];;
filter (fun x -> (x mod 2 = 0)) [];;

