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

(*dept-first search implementation*)
let rec gtree_streamize_dfs (xs: 'a gtree): 'a stream =
    let rec dfs_helper (node: 'a gtree list): 'a stream = fun() -> 
      match node with
      | [] -> StrNil
      | hd :: tl -> 
        match hd with 
        | GTnil -> dfs_helper tl()
        | GTcons(x1, x2) -> StrCons(x1, dfs_helper(list_append x2 tl)) 
    in dfs_helper [xs]
  
(*breadth-first search implementation*)
let rec gtree_streamize_bfs(xs: 'a gtree): 'a stream =
  let rec bfs_helper (node: 'a gtree list): 'a stream = fun() -> 
    match node with
    | [] -> StrNil
    | hd :: tl ->
       match hd with 
      | GTnil -> bfs_helper tl()
      | GTcons(x1, x2) -> StrCons(x1, bfs_helper(list_append tl x2)) 
  in bfs_helper [xs]
