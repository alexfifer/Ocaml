(* Alex Fifer *)


(*  Type:     int list -> int -> int list.
    preconditions: takes a list of ints and an int scalar 's' 
    postcondition: returns a list after multiplying the scalar by each of the list's ints. *)

let rec scale_row l s =
  match l with
     [] -> []
   | x :: rest -> (x * s) :: scale_row rest s ;;

   
(*  Type:     int list list -> int -> int list list.
    preconditions: takes a matrix represented by a list of int lists and an int scalar 's' 
    postcondition: returns the matrix after recursively calling scale_row on each list within.  *)

let rec matrix_scalar_multiply m s =
   match m with
       [] -> [[]]
     | x :: [] -> [scale_row x s ]
     | x :: rest -> scale_row x s :: matrix_scalar_multiply rest s ;;

