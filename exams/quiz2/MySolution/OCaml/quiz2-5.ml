(*
Q2-5: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldright. If the given list
is empty, raise the Empty exception
*)

exception Empty
let list_last(xs: 'a list): 'a =
let opt =
   list_foldright xs None (fun x acc ->
    match acc with
      | None -> Some x
      | Some _ -> acc)
in
match opt with
| Some x -> x
| None -> raise Empty
(* ************************************************ *)
