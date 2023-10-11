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

type 'a gtree =
| GTnil | GTcons of 'a * ('a gtree list)

let rec gtree_streamize_dfs (xs: 'a gtree): 'a stream =
  let rec dfs_stack stack = function
    | [] -> 
      (match stack with
      | [] -> StrNil
      | (root, children) :: rest ->
        StrCons(root, fun () -> dfs_stack rest children)
      )
    | node :: rest -> dfs_stack ((node, rest) :: stack) node
  in
  match xs with
  | GTnil -> fun () -> StrNil
  | GTcons(root, children) -> 
    StrCons(root, fun () -> dfs_stack [] children)
;;


let rec gtree_streamize_bfs (xs: 'a gtree): 'a stream =
  let rec bfs_queue queue = function
    | [] -> 
      (match queue with
      | [] -> StrNil
      | (GTnil, tail) :: rest -> bfs_queue tail rest
      | (GTcons(x, children), tail) :: rest ->
        StrCons(x, fun () -> bfs_queue tail (rest @ children))
      )
    | GTnil :: tl -> bfs_queue tl []
    | GTcons(x, children) :: tl -> StrCons(x, fun () -> bfs_queue tl (tl @ children))
  in
  match xs with
  | GTnil -> fun () -> StrNil
  | GTcons(root, children) -> 
    StrCons(root, fun () -> bfs_queue [] children)
;;
