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

let lenlist (list: 'a list) : int =
  list_foldleft list 0 (fun acc _ -> acc + 1) 

(*list_map*)
let list_map xs = foreach_to_map_list(list_foreach)(xs)

(*non-recursive version of list_nchoose*)
list_make_fwork
(fun work ->
list_foreach
(
list_foldright
(xs)([(0,[])])
(
fun x0 res ->
list_make_fwork
(
fun work ->
   list_foreach(res)
  (fun (n, xs) -> (work(n,xs); if n < n0 then work(n+1, x0 :: xs)))))
)(fun (n, xs) -> if n0 = n then work(xs)))
;;