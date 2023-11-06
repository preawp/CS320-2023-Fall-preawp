(* ****** ****** *)
(*Checked*)
#use "./../assign0.ml";;

(*
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
Note that you are not allowed to use string concatenation
or your solution is disqualified.
*)

let stringrev(cs: string): string =
  let len = string_length cs in
    string_init len (fun x -> string_get(cs,len-1-x))
(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0-5.ml] *)