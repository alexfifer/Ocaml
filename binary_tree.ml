(* Alex Fifer *)
(* Defining a type to represent a binary tree (2 children per node) *)

type 'a btree =
  Empty
| Node of 'a * 'a btree * 'a btree

type 'a bstree = { data : 'a btree ; lss : 'a -> 'a -> bool;} ;;


(* Recursive function that returns the rightmost (max value) node *)
let rec max_tree =
  function
    Empty -> None
  | Node (v, _, Empty) -> Some v
  | Node (_, _, right) -> max_tree right


(* Recursive function that returns the leftmost (min value) node *)
let rec min_tree =
  function
    Empty -> None
  | Node (v, Empty, _) -> Some v
  | Node (_, left, _) -> min_tree left

(* Recursive function that determines if the input is a binary tree.  *)
let rec isSearchTree t =
   let bigger i j =
     match j with
     | None -> true
     | (Some j') -> i >= j' in
   let smaller i j =
     match j with
     | None -> true
     | Some j' -> j' >= i in
   match t with
   | Empty -> true
   | Node (i,l,r) ->
         isSearchTree l && isSearchTree r &&
           (bigger i (maxTree l)) && (smaller i (minTree r))

(* Insert a node into a binary tree t with value x.  *)
let rec insert x t =
   match t with
   | Empty -> Node (x,Empty,Empty)
   | Node (x',l,r) ->
        if (x < x') then Node (x',insert x l,r)
        else Node (x',l,insert x r) ;;
        
let rec find t i =
   match t with
   | Empty -> false
   | Node (i',l,r) ->
       if (i = i') then true
       else if (i < i') then find l i
            else find r i

           
(* Declarations to demonstrate valid binary trees with isSearchTree.  *)
let t1 = Node(5, Node(3, Empty, Empty), Node(7, Empty, Empty));; (* isSearchTree -> true *)
let t2 = Node(3, Node(2, Empty, Empty), Node(4, Empty, Empty));; (* isSearchTree -> true *)
let t3 = Node(1, Node(9, Empty, Empty), Node(7, Empty, Empty));; (* isSearchTree -> false *)



(* treemap has type 'a btree -> ('a -> 'b) -> 'b btree  
   Preconditions: f must be a function that takes 1 argument. It will operate on each node's v.
                  root must be type 'a btree and it's v must be an acceptable argument in f.
   Postcondition: The resulting tree is a btree of whatever type the function f produces.    *)
let rec treemap root f =
  match root with
  | Empty -> Empty
  | Node(v, left, right) ->  Node((f v), treemap left f, treemap right f) ;;


(* Sample calls to treemap *)
treemap (Node (4, Node (2, Empty, Empty), Node (5, Empty,Empty))) (fun x -> x + 3);;
treemap (Node (4, Node (2, Empty, Empty), Node (5, Empty,Empty))) (fun x -> (x mod 2 = 0));;

(* sumTree has type:  int btree -> int  *)
let rec sumTree t =
    match t with
    | Empty -> 0
    | Node (i,l,r) -> i + sumTree l + sumTree r ;;
    
    
(*   cont_sumTree : int btree -> (int -> 'a) -> 'a   *)
let rec cont_sumTree root f =
  match root with
  | Empty -> (f 0)
  | Node (i, l, r) -> cont_sumTree l (fun x -> f(i + x))
  
  
