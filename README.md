### Ocaml
A repository of basic functions

### Usage
Start the opam interpreter with `ocaml`.

commands are terminated by the delimiter `;;`.

run scripts with `#use "filename.ml" ;;`.

exit the opam interpreter with `exit 0;;`.
### Sample 
```
(base) alex@fifer:~/code/Ocaml$ ocaml
        OCaml version 4.12.0

# 1;;
- : int = 1
# 1 + 1 ;;
- : int = 2
# #use "countdown.ml" ;;
val fromMtoN : int -> int -> int list = <fun>
# fromMtoN 9 3 ;;
- : int list = [9; 8; 7; 6; 5; 4; 3]
# #use "search_database.ml" ;;
val find_salary : (string * string * float) list -> string -> float = <fun>
val find_phno : (string * string * float) list -> string -> string = <fun>
# exit 0 ;;
(base) alex@fifer:~/code/Ocaml$ 
```
