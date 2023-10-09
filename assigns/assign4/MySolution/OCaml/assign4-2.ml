#use "./../../../../classlib/OCaml/MyOCaml.ml";; 
(*
//
Assign4-2:
//
HX-2023-10-05: 10 points
//
Please enumerate all the pairs of natural
numbers. Given pairs (i1, j1) and (i2, j2),
(i1, j1) should be enumerated ahead of (i2, j2)
if i1+j1 < i2+j2.
//
let theNatPairs: (int*int) stream = fn () => ...
//
*)
let theNatPairs : (int * int) stream =
  let rec helper (i, j) =
    if j = 0 then
      StrCons ((i, j), fun () -> helper (0, i + 1))
    else
      StrCons ((i, j), fun () -> helper (i + 1, j - 1))
  in
  fun () -> helper (0, 0)

