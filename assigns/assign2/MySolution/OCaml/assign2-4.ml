#use "./../../../../classlib/OCaml/MyOCaml.ml";; 
#use "./../../assign2.ml";;
(*
//
Assign2-4: 10 points
//
Please given a combinator-based implementation of
string_sepjoin_list:
let
string_sepjoin_list
(sep: string)(xs: string list): string = ...

For instance,
string_sepjoin_list(",")(["1","22","333"]) = "1,22,333"
For instance,
string_sepjoin_list(";;")(["11","22","33"]) = "11;;22;;33"
*)
(* ****** ****** *)
let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = 
  fun xs work ->
    let _ = foldleft(xs)(0)(fun i x -> (work(i)(x); i+1)) in ()
    
let
list_iforeach =
fun xs -> foldleft_to_iforeach(list_foldleft)(xs)
(* ****** ****** *)
let string_sepjoin_list(sep: string)(xs: string list): string = 
  string_make_fwork(fun work -> list_iforeach (xs) (fun i cs -> (if i > 0 then string_foreach(sep)(work); string_foreach(cs)(work))))