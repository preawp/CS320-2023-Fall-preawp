#use "./../../../../classlib/OCaml/MyOCaml.ml";; 
#use "./../../assign3.ml";;
(*
Assign3-3:
HX-2023-09-26: 10 points
//
The function [list_nchoose(xs)(n0)]
returns all the subsequences of xs that are
of length n0.
//
let rec
list_nchoose
(xs: 'a list)(n0: int): 'a list list =
//
Please give a NON-RECURSIVE implementation of
list_nchoose based on list-combinators. Note that
the order of the elements in a list representation
of a subsequenc is SIGNIFICANT. For instance, [1;2]
and [2;1] are DIFFERENT.
//
*)

(*customized list_map*)
let rec list_map f lst =
  match lst with
  | [] -> []
  | x :: xs -> f x :: list_map f xs

(*non recursive version of list_nchoose*)
let list_nchoose (xs: 'a list)(n0: int): 'a list list =
  let rec combine n xs =
    if n = 0 then [[]]
    else
      match xs with
      | [] -> []
      | h :: t ->
        let with_h = list_map (fun l -> h :: l) (combine (n - 1) t) in
        let without_h = combine n t in
        list_append with_h without_h
  in
  combine n0 xs