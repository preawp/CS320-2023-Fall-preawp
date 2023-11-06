(*checked*)
#use "./../../../../classlib/OCaml/MyOCaml.ml";; 
#use "./../../assign2.ml";;
(* ****** ****** *)

type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist
(*
//
Assign2-1: 10 points
//
Please implement mylist_length based
on pattern matching that computes the
length of a given mylist.
//
let rec
mylist_length(xs: 'a mylist): int = ...
//
*)

(*return length of list based on my type mylist*)
let rec mylist_length (xs: 'a mylist): int=
  match xs with 
  | MyNil -> 0
  | MyCons(_,t) -> 1 + mylist_length(t)
  | MySnoc(t,_ ) -> 1 + mylist_length t
  | MyReverse(t) -> mylist_length t
  | MyAppend2(h,t) -> mylist_length h + mylist_length t
