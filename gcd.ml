(* Alex Fifer *)


(* Returns the greatest common divisor of two integer arguments *)
(*  Type:          int -> int -> int
    preconditions: takes 2 positive ints in any order. If 'a' is smaller, 
                   gcd is called again in the other order.
    postcondition: output (int) is the greatest common divisor of the 2 
                   original args    
*)
let rec gcd a b =
   if a = b then a
   else 
      if a < b then gcd b a
      else gcd (a - b) b ;;

(* Returns a pair of ints representing the input pair's reduced fraction *)
(*  reduced_form has type  int * int -> int * int = <fun>
    preconditions: takes a pair/tuple of positive ints representing a fraction.
                   gcd must be defined.
    postcondition: output (int pair/tuple) has the same fractional value as the input pair,
                   but the "fraction" is now in reduced form.   *)

let rec reduced_form =
   function
     | (a, b) -> if gcd a b = 1 then (a, b)
                 else reduced_form (a / gcd a b, b / gcd a b) ;;
