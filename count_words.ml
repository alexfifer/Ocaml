(* Alex Fifer *)

(* add 1 to the wordcount *)
let rec processWord l k =
  match l with
  | [] -> [(k, 1)]
  | ((j: string), i) :: t -> if j < k then (j, i) :: processWord t k 
                             else if j = k then (j, i + 1) :: t
                                  else (k, 1) :: (j, i) :: t ;;

let processOneList l = List.fold_left processWord [] l ;;


(* assimilateWordCount : (string * int) list -> string * int -> (string * int) list *)
let rec assimilateWordCount lst pair =
  match lst with
  | [] -> [pair]
  | ((j: string), i) :: t -> match pair with 
                             (str, count) -> if str < j then pair :: (j, i) :: t
                                             else if str = j then (str, i + count) :: t
                                                  else (j, i) :: assimilateWordCount t pair ;;

let assimilateWCList wcs wcl = List.fold_left assimilateWordCount wcs wcl ;;

let wordCounts ls = List.fold_left assimilateWCList [] (List.map processOneList ls) ;;


(* Sample calls to processWord *)
processWord [] "ball";;
processWord [("ball", 1); ("the", 1); ("threw", 1)] "girl";;
processWord [("ball", 1); ("girl", 1); ("the", 1); ("threw", 1)] "the";;

(* Sample calls to processOneList *)
processOneList ["the"; "girl"; "threw"; "the"; "ball"];;

(* Sample call to assimilateWCList *)
assimilateWCList [("ball", 1); ("girl", 1); ("the", 2); ("threw", 1)]
                 [("a", 1); ("ball", 1); ("bat", 1); ("by", 1);
                  ("struck", 1); ("the", 1); ("was", 1)] ;;
                 
(* Sample calls to assimilateWordCount *)
assimilateWordCount [("ball", 1); ("girl", 1); ("the", 2); ("threw", 1)] ("a",1) ;;
assimilateWordCount [("ball", 1); ("girl", 1); ("the", 2); ("threw", 1)] ("the",2) ;;
