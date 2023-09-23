#use "./../../../../classlib/OCaml/MyOCaml.ml";; 
#use "./../../assign2.ml";;
(*
//
Assign2-2: 10 points
//
Please implement mylist_get_at based
on pattern matching that computes the
element of a given mylist at a given
position.
//
You should call mylist_subscript_exn if
the give position is out-of-bounds.
//
let rec
mylist_get_at(xs: 'a mylist)(i0: int): 'a = ...
//
*)

(* ****** ****** *)
let rec mylist_length(xs: 'a mylist): int = 
  match xs with 
  | MyNil -> 0 
  | MyCons(_, t) -> 1 + mylist_length t
  | MySnoc(t, _) -> 1 + mylist_length t
  | MyReverse(t) -> mylist_length t
  | MyAppend2(t1, t2) -> mylist_length t1 + mylist_length t2
  
let rec mylist_get_at(xs: 'a mylist)(i0: int): 'a =
  match xs, i0 with
  | MyCons(x1, _), 0 -> x1
  | MySnoc(_, x1), 0 -> x1
  | MyCons(_, rest), _ -> mylist_get_at(rest)(i0 - 1)
  | MySnoc(rest, _), _ -> mylist_get_at(rest)(i0 - 1)
  | MyReverse(rlst), _ -> mylist_get_at(rlst)(i0)
  | MyAppend2(lst1, lst2), _ ->
    let len1 = mylist_length(lst1) in
    if i0 < len1 then mylist_get_at(lst1)(i0)
    else mylist_get_at(lst2)(i0 - len1)
  | _ -> mylist_subscript_exn()
;;

