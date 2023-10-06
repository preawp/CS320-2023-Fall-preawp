
(* ************************************************ *)

(*
Q2-5: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldright. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)

exception Empty

let list_last(xs: 'a list): 'a =
  match xs with
  | [] -> raise Empty
  | h :: t ->
      let result = ref None in
      list_foldright t () (fun x () -> if !result = None then result := Some x
    );
     match !result with
    | Some x -> x
    | None -> raise Empty
