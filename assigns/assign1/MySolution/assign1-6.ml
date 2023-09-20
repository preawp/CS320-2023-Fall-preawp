#use "./../../../classlib/OCaml/MyOCaml.ml";;


(*
assign1-6: 20 bonus points
A 4-letter sequence abcd is 1324-like
if a < c < b < d holds. For instance, 1234 is
not 132-like; but 2547 is 1324-like.

A string is 1324-avoid if there is no subsequence
abc in this string that is 1324-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 1324-avoid;
For instance, 987654321 is 1324-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 1657 is 1324-like.

Please implement a function string_avoid_1324 that
checks if a given string is 1324-avoid; the function
returns true if and only if the given string is 1324-
avoid.

fun string_avoid_1324(cs: string): bool
*)

(*a function to check the 1324-like pattern*)
let is_1324_like (a: char) (b: char) (c: char) (d: char) =
  a < c && c < b && b < d

(*similar to string_avoid_132, we recursively check if it is 1324-like*)
let string_avoid_1324 (cs: string): bool =
  let length = string_length cs in
  if length < 4 then true
  else 
    let rec string_avoid_1324_helper (i: int) (j: int) (k: int) (l: int): bool =
     if i >= length then true
     else if j >= length then string_avoid_1324_helper (i + 1) (i + 2) (i + 3) (i + 4)
     else if k >= length then string_avoid_1324_helper i (j + 1) (j + 2) (j + 3)
     else if l >= length then string_avoid_1324_helper i j (k + 1) (k + 2)
     else if is_1324_like (string_get_at cs i) (string_get_at cs j) (string_get_at cs k) (string_get_at cs l) then false
     else string_avoid_1324_helper i j k (l + 1)
    in
   string_avoid_1324_helper 0 1 2 3
