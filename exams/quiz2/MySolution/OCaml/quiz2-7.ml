
(* ****** ****** *)

(* end of [CS320-2023-Fall-classlib-MyOCaml.ml] *)
(* ************************************************ *)

(*
Q2-7: 10 points

The following implementation of list_append is not tail-recursive.
Please give an implementation of list_append that is tail-recursive.

Note that you can only use pattern matching and list_foldleft in your
implementation.
 
let rec
list_append(xs: 'a list)(ys: 'a list) =
match xs with
  [] -> ys | x1 :: xs -> x1 :: list_append(xs)(ys)
*)

(* ************************************************ *)
let list_append (xs: 'a list)(ys: 'a list) =
      let rev_append  xs ys = list_foldleft xs ys (fun acc x -> x :: acc) in
      let rev xs = rev_append xs [] in 
      rev_append(rev xs) ys

