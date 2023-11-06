(* ****** ****** *)
(*Checked*)
#use "./../assign0.ml";;
(*
Assign0-1: 10 points
Please find the first integer N such that the
evaluation of fact(N) in OCaml returns '0' (due
to arithmetic overflow.
*)

(* ****** ****** *)

(*solution given*)
let rec fact(n : int): int =
  if n>0 then fact(n-1)* n
  else 1

let rec find(x:int): int =
  if fact(x) = 0 then x 
  else find(x+1)

let myans = find(0)

(* end of [CS320-2023-Fall-assign0-1.ml] *)
