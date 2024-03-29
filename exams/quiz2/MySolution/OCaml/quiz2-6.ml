
(* ************************************************ *)

(*
Q2-6: 10 points

The function list_reverse return the reverse of a given list.
Please give an implementation of list_reverse based on list_foldright
(not list_foldleft).
*)

(* ************************************************ *)

let list_reverse(xs: 'a list): 'a list = 
  let snoc = list_foldright xs [] (fun x acc -> x :: acc) in  
  list_foldright xs [] (fun x acc -> snoc acc x)