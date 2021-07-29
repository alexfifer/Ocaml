(*Alex Fifer *)

(* take a list of pairs and "unzip" them into a pair of lists.  *)
(*  type ('a * 'b) list -> 'a list * 'b list. unzip returns a tuple containing two
    lists. The second 'let' allows the first pair in the list, a and b, to be 'cons'ed
    to the returned pair from 'unzip tl'   *)

let rec unzip =
    function
      | [] -> ([], [])
      | (a, b) :: tl -> 
         let atl, btl = unzip tl
         in a::atl, b::btl
         
