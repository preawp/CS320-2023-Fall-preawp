#use "./../../../../classlib/OCaml/MyOCaml.ml";; 
#use "./../../assign3.ml";;
(*
Assign3-1:
HX-2023-09-26: 10 points
A matrix can be represented as a list of lists.
For instance, [[1;2;3];[4;5;6];[7;8;9]] represents
the following 3x3 square matrix:
1 2 3
4 5 6
7 8 9
Please implement matrix_transpose that returns
the transpose of a given matrix:
let rec
matrix_transpose(xss: 'a list list): 'a list list
For instance, the transpose of the above matrix
is given as follows:
1 4 7
2 5 8
3 6 9
You are allowed to define recursive functions to
solve this problem.
*)

(* ****** ****** *)


(*customized list_map*)
let rec list_map f lst =
  match lst with
  | [] -> []
  | x :: xs -> f x :: list_map f xs

(* ****** ****** *)
(*func to get head of the list*)
let list_hd lst =
  match lst with
  | [] -> raise (Failure "No head")
  | x :: _ -> x
;;
(*func to get tail of the list*)
let list_tl lst =
  match lst with
  | [] -> raise (Failure "No tail")
  | _ :: x -> x
;;

(*helper function to transpose the matrix*)
let rec transpose_helper xss =
  match xss with
  | [] -> []
  | [] :: _ -> []
  | _ -> list_map list_hd xss :: transpose_helper (list_map list_tl xss)

(*main function that recursively call to helper*)
let rec matrix_transpose(xss: 'a list list): 'a list list= 
  match xss with
  | [] -> []
  | [] :: _ -> []
  | _ -> transpose_helper xss
