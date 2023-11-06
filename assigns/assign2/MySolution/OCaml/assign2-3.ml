(*kinda*)
#use "./../../../../classlib/OCaml/MyOCaml.ml";; 
#use "./../../assign2.ml";;

(*
//
Assign2-3: 10 points
//
Please implement foldleft_to_iforeach that turns a
foldleft-loop into a iforeach-loop:
let
foldleft_to_iforeach
(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = ...
*)

(* converts by applying the given function f to each element in the collection xs,
    while ignoring the final acc*)
let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = 
  fun xs work ->
    let _ = foldleft(xs)(0)(fun i x -> (work(i)(x); i+1)) in ()
    


