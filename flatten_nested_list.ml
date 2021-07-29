(* Alex Fifer *)


(* A type to represent nested lists  *)
type 'a nestedList =
    | One of 'a 
    | Many of 'a nestedList list;;

(* Recursively "flatten" a nested list  *)
let flatten l =
  let rec loop a = function
    | [] -> List.rev a
    | (One h)::rest -> loop (h :: a) rest
    | (Many h)::rest -> loop (loop a h) rest
  in
    loop [] l

(* Sample call to flatten *)
(*
flatten [[8; 9; [11]]] [];;
 *)
