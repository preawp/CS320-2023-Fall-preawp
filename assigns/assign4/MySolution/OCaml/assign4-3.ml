#use "./../../../../classlib/OCaml/MyOCaml.ml";; 
(*
//
Assign4-3:
//
HX-2023-10-05: 10 points
//
Please enumerate a gtree in the manner of
depth-first search:
//
let rec (* 5 points *)
gtree_streamize_dfs(xs: 'a gtree): 'a stream
//
Please enumerate a gtree in the manner of
breadth-first search:
//
let rec (* 5 points *)
gtree_streamize_bfs(xs: 'a gtree): 'a stream
//
*)

(* ****** ****** *)

type 'a gtree =
| GTnil | GTcons of 'a * ('a gtree list)

(* ****** ****** *)
let rec gtree_streamize_dfs (xs: 'a gtree): 'a stream =
  let rec dfs_queue xs =
    match xs with
    | GTnil -> StrNil
    | GTcons (x, children) ->
      let child_streams = List.map gtree_streamize_dfs children in
      let child_stream = List.fold_right (fun s acc -> fun () -> StrCons (s(), acc)) child_streams (fun () -> StrNil) in
      fun () -> StrCons (x, child_stream)
  in
  fun () -> dfs_queue xs
;;

let rec gtree_streamize_bfs (xs: 'a gtree): 'a stream =
  let rec bfs_queue queue =
    match queue with
    | [] -> StrNil
    | GTnil :: rest -> bfs_queue rest
    | GTcons (x, children) :: rest ->
      let child_streams = List.map gtree_streamize_bfs children in
      let child_stream = List.fold_right (fun s acc -> fun () -> StrCons (s(), acc)) child_streams (fun () -> StrNil) in
      fun () -> StrCons (x, bfs_queue (rest @ children))
  in
  fun () -> bfs_queue [xs]
;;
