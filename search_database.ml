(* Alex Fifer *)


(* Search a simple database consisting of a list of tuples. *)

(*  Type:     (string * string * float) list -> string -> float 
    preconditions: 'db' is a database represented by a (string * string * float) list.
                   'nm' is the contact name (a string).
    postcondition: When the name is found in 'db', find_salary returns 'sal' (a float).
                   The exception is the base case, and will always be raised if the end 
                   of the list is reached without a 'name' matching 'nm'.    
*)
let rec find_salary db nm =
  match db with
    ((name: string), (num: string), (sal: float)) :: rest ->
         if name = nm then sal
         else find_salary rest nm
  | [] -> raise Not_found ;;
(*  note: The exception was suggested as a way of dealing with a failed 
          query of a list by Jason Hickey in "Introduction to Objective Caml"  *)

          
          
(*  Type:      (string * string * float) list -> string -> string 
    preconditions: 'db' is the database from above.
                   'nm' is the name (a string) of the desired contact.
    postcondition: When the name is found in 'db', find_phno returns 'num' (a string).
                   The exception is the base case, and will always be raised if the end 
                   of the list is reached without a 'name' matching 'nm'.            *)

let rec find_phno db nm =
  match db with
    ((name: string), (num: string), (sal: float)) :: rest ->
         if name = nm then num
         else find_phno rest nm
  | [] -> raise Not_found ;;
  
