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
let list_nchoose (xs: 'a list)(n0: int): 'a list list =
  let combine n xs =
    let stack = ref [(n0, xs, [])] in
    let result = ref [] in
    while !stack <> [] do
      match !stack with
      | [] -> ()
      | (n', xs', current) :: rest ->
        stack := rest;
        if n' = 0 then
          result := list_append [list_reverse current] !result
        else
          match xs' with
          | [] -> ()
          | h :: t ->
            stack := (n' - 1, t, h :: current) :: (n', t, current) :: !stack
    done;
    !result
  in
  if n0 <= 0 then [[]]
  else if n0 > lenlist xs then []
  else combine n0 xs
