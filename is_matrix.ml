(* Alex Fifer *)


(*  A matrix may have any natural number of rows/columns, but must be "rectangular".
    i.e. once the first 'row list' is counted, all remaining row lists must have the
    same length.  *)

(*  Type:    int list list -> bool.
    preconditions: takes a list of int lists OR an empty list    
    postcondition: output is a bool which is true if each "row list" is the same length.
                   If a row does not match the length of the previous row, bool is false.  *)
let rec is_matrix =
  function
    | [] -> true
    | (x : int list) :: [] -> true
    | x :: y :: rest -> if List.length x = List.length y then is_matrix (y :: rest)
                        else false ;;
