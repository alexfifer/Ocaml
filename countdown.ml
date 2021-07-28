(* Alex Fifer *)


(* Returns the list of ints counting down from m to n.  *)
(*  Type:    int -> int -> int list = <fun>
    preconditions: takes 2 ints in any order. 
                   If m is smaller, output is an empty list.   *)

let rec fromMtoN m n =
   if m < n then []
   else m:: fromMtoN (m - 1) n ;;
