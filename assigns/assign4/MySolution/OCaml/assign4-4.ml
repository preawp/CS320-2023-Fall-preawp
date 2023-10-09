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

let list_streamize xs =
  let stream_of_list = int1_map_stream (List.length xs) (fun i -> List.nth xs i) in
  fun () -> stream_of_l
  
let rec insert_everywhere (x : 'a) (xs : 'a list) : 'a list list =
  match xs with
  | [] -> [[x]]
  | y :: ys as lst -> (x :: lst) :: List.map (fun zs -> y :: zs) (insert_everywhere x ys)

let rec permutations (xs : 'a list) : 'a list list =
  match xs with
  | [] -> [[]]
  | x :: xs' ->
    let perms = permutations xs' in
    List.flatten (List.map (insert_everywhere x) perms)

let stream_permute_list (xs : 'a list) : 'a list stream =
  let all_perms = permutations xs in
  let perm_stream = list_streamize all_perms in
  fun () -> perm_stream
