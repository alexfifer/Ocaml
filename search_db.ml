(* Alex Fifer *)

(* Type for storing employee info *)
type empItemTy =
  { name : string;
    phone_no : string;
    salary : float; }

(* Binding the identifier 'smalldb' to the sample data *)
let smalldb = [{name = "John";
                phone_no = "x3456";
                salary = 50.1;};
               {name = "Jane";
                phone_no = "x1234";
                salary = 107.3;};
               {name = "Joan";
                phone_no = "unlisted";
                salary = 12.7; }]

            
(* find_salary is now of type (empItemTy list) -> string -> float option  *)

let rec find_salary db nm =
  match db with
    {name = n; phone_no = p; salary= sal} :: rest ->
         if n = nm then Some sal
         else find_salary rest nm
  | [] -> None;;


(* find_phno is now of type (empItemTy list) -> string -> string option *)

let rec find_phno db nm =
  match db with
    {name = n; phone_no = p; salary = sal} :: rest ->
         if n = nm then Some p
         else find_phno rest nm
  | [] -> None ;;

(* tests for find_ functions *)
find_salary smalldb "Jane";;
find_phno smalldb "John";;

