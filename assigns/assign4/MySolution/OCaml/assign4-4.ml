#use "./../../../../classlib/OCaml/MyOCaml.ml";; 
(*
//
Assign4-4:
//
HX-2023-10-05: 20 points
//
Please enumerate all the permuations of a
given list. The enumeration is required to be
in order. For instance, say xs = [1;2;3], then
the enumeration should be of the following order
[1;2;3], [1;3;2], [2;1;3], [2;3;1], [3;1;2], [3;2;1].
//
let list_permute(xs: 'a list): 'a list stream
*)


let rec list_to_stream (lst: 'a list): 'a stream =
  match lst with
  | [] -> fun () -> StrNil
  | x::xs -> fun () -> StrCons(x, list_to_stream xs)

let rec permutations (xs: 'a list): 'a list list =
  match xs with
  | [] -> [[]]
  | x::xs' ->
    let perms = permutations xs' in
    let rec insert_everywhere (x: 'a) = function
      | [] -> [[x]]
      | y::ys -> (x :: y :: ys) :: List.map (fun p -> y :: p) (insert_everywhere x ys)
    in
    List.flatten (List.map (fun p -> insert_everywhere x p) perms)

let list_permute (xs: 'a list): 'a list stream =
  let all_perms = permutations xs in
  list_to_stream all_perms
