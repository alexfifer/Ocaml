(* Alex Fifer *)


(* Returns the input list with only the odd-indexed items remaining *)

(*  Type:     'a list -> 'a list
    preconditions: takes any type list.
    postcondition: returns a list of the same type containing all odd index
                   items starting with 1st (1st, 3rd, 5th,...odd-index).  *)

let rec every_odd =
   function
     | [] -> []
     | x :: [] -> [x]
     | x :: y :: tl -> x :: every_odd tl  ;;
 
