#use "./../../../../classlib/OCaml/MyOCaml.ml";; 

(*
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

type 'a gtree =
| GTnil 
| GTcons of 'a * ('a gtree list)

let rec gtree_streamize_dfs (xs: 'a gtree): 'a stream =

    match xs with
    | GTnil -> StrNil
    | GTcons(x1, xs') -> StrCons(x1,  gtree_streamize_dfs xs')