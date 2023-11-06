(*kinda*)

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
(*return length of list based on my type mylist*)
let rec mylist_length(xs: 'a mylist): int = 
  match xs with 
  | MyNil -> 0 
  | MyCons(_, t) -> 1 + mylist_length t
  | MySnoc(t, _) -> 1 + mylist_length t
  | MyReverse(t) -> mylist_length t
  | MyAppend2(t1, t2) -> mylist_length t1 + mylist_length t2

(** Reverses a generic list**)
let rec list_reverse_mylist (xs: 'a mylist): 'a mylist =
  match xs with
  | MyNil -> MyNil
  | MyCons (x, t) -> MyCons (x, list_reverse_mylist t)
  | MySnoc (t, x) -> MySnoc (list_reverse_mylist t, x)
  | MyReverse l -> l 
  | MyAppend2 (t1, t2) -> MyAppend2 (list_reverse_mylist t2, list_reverse_mylist t1)
  


(*computes the element of a given mylist at a given position, otherwise exception*)
let rec mylist_get_at (xs: 'a mylist) (i0: int): 'a =
  if i0 < 0 then mylist_subscript_exn ()
  else 
   match xs with
    | MyNil -> mylist_subscript_exn ()
    | MyCons (x, rest) ->
      if i0 = 0 then x 
      else mylist_get_at rest (i0 - 1)
    | MySnoc (init, x) ->
      let len_init = mylist_length init in
      if i0 = len_init then x
      else mylist_get_at init i0
    | MyReverse l -> mylist_get_at (list_reverse_mylist l) i0
    | MyAppend2 (l1, l2) ->
      let len_l1 = mylist_length l1 in
      if i0 < len_l1 && i0 >= 0 then mylist_get_at l1 i0
      else mylist_get_at l2 (i0 - len_l1)
   
