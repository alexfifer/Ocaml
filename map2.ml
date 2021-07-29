(* Alex Fifer *)

(* Map a function over the elements of two seperate lists.  *)

(* map2 has type ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list   *)
(* Preconditions: the function must be able to take the elements of l1, l2 as arguments.
   Postcondition: the returned list is the result of the function, f, having mapped over 
                  corresponding pairs of elements in l1, l2 until one of the lists is empty. *)
let rec map2 f l1 l2 =
  match l1 with
    [] -> []
  | (h1 :: t1) -> match l2 with
                   [] -> []
                 | (h2 :: t2) -> (f h1 h2) :: map2 f t1 t2;;

(* sample calls to map2 *)
map2 (fun x y -> x + y) [1;2;3] [4;5;6;7];;
map2 (fun x y -> x + y) [1;2;3;4] [5;6;7];;


(* A sample use of map2 that zips two lists together.  *)
(* Type:   'a list -> 'b list -> ('a * 'b) list 
   precondition: map2 must be defined.         *)
let zip l1 l2 = map2 (fun h1 h2 -> (h1, h2)) l1 l2 ;;

