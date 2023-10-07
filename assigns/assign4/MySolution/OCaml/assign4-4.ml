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

(* ****** ****** *)

let rec list_permute (xs: 'a list): 'a list stream =
  let rec insert_all_positions x xs =
    match xs with
    | [] -> [[x]]
    | hd :: tl ->
      (x :: xs) :: (List.map (fun p -> hd :: p) (insert_all_positions x tl))
  in
  match xs with
  | [] -> fun () -> StrNil
  | x :: rest ->
    let insertions = List.map (insert_all_positions x) (list_permute rest) in
    let permutations = List.flatten insertions in
    fun () -> StrCons (permutations, list_permute rest)
;;

let rec stream_to_list n s =
  if n <= 0 then []
  else match s() with
       | StrNil -> []
       | StrCons (x, xs) -> x :: stream_to_list (n - 1) xs
;;

let () =
  let xs = [1; 2; 3] in
  let permute_stream = list_permute xs in
  let n = List.length xs * (List.length xs - 1) in
  let permutations = stream_to_list n permute_stream in
  List.iter (fun perm -> Printf.printf "[%s]\n" (String.concat "; " (List.map string_of_int perm))) permutations
