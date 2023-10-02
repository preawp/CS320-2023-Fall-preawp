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
type board_t = int * int * int * int * int * int * int * int

let is_safe board =
  let rec is_safe_helper queens row col =
    match queens with
    | [] -> true
    | (r, c) :: qs ->
      if col = c || abs (row - r) = abs (col - c) then false
      else is_safe_helper qs row col
  in
  let row = List.length board in
  let col = List.hd board in
  is_safe_helper board row col

let initial_board = [0]

let next_col col = col + 1

let rec solve_queen_puzzle () =
  let solutions = ref [] in
  let board_stack = Stack.create () in
  Stack.push initial_board board_stack;

  while not (Stack.is_empty board_stack) do
    let board = Stack.pop board_stack in
    let row = List.length board in
    let col = ref (next_col (List.hd board)) in

    while !col < 8 do
      if is_safe board then begin
        if row = 7 then
          solutions := (List.rev board) :: !solutions
        else
          Stack.push (!col :: board) board_stack;
      end;
      col := next_col !col;
    done;
  done;
  !solutions

let queen8_puzzle_solve () =
  let solutions = solve_queen_puzzle () in
  List.map (fun board -> Array.of_list board) solutions
1

