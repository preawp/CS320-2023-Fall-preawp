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

let rec cantor_pairs i j =
  fun () ->
    let next =
      if i = 0 then (0, j + 1) else (i - 1, j + 1)
    in
    StrCons((i, j), cantor_pairs (fst next) (snd next))

let theNatPairs: (int * int) stream = cantor_pairs 0 0
let rec take_n_pairs n s =
  match (n, s()) with
  | (0, _) | (_, StrNil) -> []
  | (n, StrCons (pair, rest)) -> pair :: take_n_pairs (n - 1) rest

