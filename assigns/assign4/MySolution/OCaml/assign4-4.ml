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

let rec permutation lst = 
  let rec stream_work s f1 f2 =
    match s with
    | StrNil -> f2()
    | StrCons (h, ss)-> StrCons(f1 h, fun () -> stream_work (ss ()) f1 f2) in 

  match lst with
  | [] -> fun () -> StrCons([], fun () -> StrNil)
  | lst1 -> 
  let rec helper lst1 lst2 =
   match lst1 with
    | [] -> StrNil
    | h::lst1 -> 
        let list_add x lst = 
          x :: lst in
        let element = list_append (list_reverse lst2) lst1 in 
        stream_work (permutation element ()) (list_add h) (fun () -> helper lst1 (list_add h lst2)) 

in
  fun() -> helper lst1 []

let list_permute(xs: 'a list): 'a list stream =
  let result = permutation(xs)
in result
;;