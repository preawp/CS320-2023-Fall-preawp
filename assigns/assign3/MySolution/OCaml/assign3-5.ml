#use "./../../../../classlib/OCaml/MyOCaml.ml";; 
#use "./../../assign3.ml";;
(* ****** ****** *)
(*
//
Assign3-5:
HX-2023-09-28: 30 points (bonus)
//
Here is an implementation of the famous 8-queen puzzle:
https://ats-lang.sourceforge.net/DOCUMENT/INT2PROGINATS/HTML/x631.html
//
Please give a NON-RECURSIVE implementation that solves the 8-queen puzzle.
//
Hint: Please think of programming in terms of combinators.
//
//
type board_t =
int * int * int * int * int * int * int * int
//
fun
queen8_puzzle_solve(): board_t list =
(*
returns a list of boards consisting of all the solutions to the puzzle.
*)
*)
(* ****** ****** *)


(* Define the type board_t *)
type board_t = int * int * int * int * int * int * int * int

(* Constant n*)
let n = 8

(*list_map*)
let list_map xs = foreach_to_map_list(list_foreach)(xs)

(* board_set, updates the position of a queen on the chessboard *)
let board_set bd row col =
	let (x0,x1,x2,x3,x4,x5,x6,x7) = bd in
  match row with
  | 1 -> (col, x1, x2, x3, x4, x5, x6, x7)
  | 2 -> (x0, col, x2, x3, x4, x5, x6, x7)
  | 3 -> (x0, x1, col, x3, x4, x5, x6, x7)
  | 4 -> (x0, x1, x2, col, x4, x5, x6, x7)
  | 5 -> (x0, x1, x2, x3, col, x5, x6, x7)
  | 6 -> (x0, x1, x2, x3, x4, col, x6, x7)
  | 7 -> (x0, x1, x2, x3, x4, x5, col, x7)
  | 8 -> (x0, x1, x2, x3, x4, x5, x6, col)
  | _ -> bd

(* board_get, get a value from the chessboard. *)
let board_get bd row =
	let (x0,x1,x2,x3,x4,x5,x6,x7) = bd in
  match row with
  | 1 -> x0
  | 2 -> x1
  | 3 -> x2
  | 4 -> x3
  | 5 -> x4
  | 6 -> x5
  | 7 -> x6
  | 8 -> x7
  | _ -> raise (Invalid_argument "Row out of bounds")

(* safety1, tests whether a queen piece on 
	 row i0 and column j0 can capture another one on row i and column j. *)
let safety1 i j row col =
  (j != col) && (abs (i - row) != abs (j - col))

(* safety2, safety_test2 tests whether a queen piece on row i0 and column j0 can capture any
	  other pieces on a given board with a row number less than or equal to i. *)
let safety2 i j bd =
  int1_forall (i - 1) (fun i0 -> safety1 i j (i - 1 - i0) (board_get bd (i - 1 - i0)))

(* val_sols, func to find all valid columns for a particular row. *)
let valid_cols pos acc bd row =
  list_foldleft pos acc (fun r0 x0 ->
    if safety2 row x0 bd then list_append r0 [board_set bd row x0]
		 else r0)

(*valid_sols, helper function to search for valid sols *)
let valid_sols pos b row =
  list_foldleft (list_map b (fun x -> valid_cols pos [] x row)) [] (fun r0 x -> list_append x r0)

(* queen8_puzzle_solve, solve the 8-queen puzzle. *)
let queen8_puzzle_solve () =
  let pos = [1; 2; 3; 4; 5; 6; 7; 8] in
  let init_board = (-1, -1, -1, -1, -1, -1, -1, -1) in
  list_foldleft pos [init_board] (fun r0 x -> valid_sols pos r0 x)
