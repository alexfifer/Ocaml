(* Alex Fifer *)


(* Returns a list of any type with the nth element as the head. *)
(* Type:    'a list -> int -> 'a list
   preconditions: takes a list of any type and a positive int
   postcondition: output is the list 'l' with the nth element at the head. *)
   
let rec get_next l n =
   if n = 1 then l
   else match l with
     | [] -> []
     | x :: rest -> get_next rest (n - 1)

     
(* Type:    'a list -> int -> 'a list
   preconditions: takes a list of any type and a positive int.
                  get_next must be defined.
   postcondition: output is a list containing every nth element from 'l'.
                  getNext is called to move the head 'n' elements.   *)

let rec every_nth l n =
  let next = get_next l n
  in
     match next with
       | [] -> []
       | x :: rest -> x :: every_nth rest n ;;


let every_third l = every_nth l 3 ;;
