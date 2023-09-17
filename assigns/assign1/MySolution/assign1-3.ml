#use "./../../../classlib/OCaml/MyOCaml.ml";; 
(*
assign1-3: 10 points
A 3-letter sequence abc is 132-like
if a < c < b holds. For instance, 123 is
not 132-like; but 596 is 132-like.

A string is 132-avoid if there is no subsequence
abc in this string that is 132-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 132-avoid;
For instance, 987654321 is 132-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 165 is 132-like.

Please implement a function string_avoid_132 that
check if a given string is 132-avoid; the function
returns true if and only if the given string is 132-
avoid.

fun string_avoid_132(cs: string): bool
*)

(*helper function to check if any 3 chars have 132-like pattern*)
let is_132_like (a: char) (b: char) (c: char) =
  a < c && c < b

(*recursively use helper function that pick all possible 3 chars by indexes and check whether 132 pattern exists*)
let string_avoid_132 (cs: string): bool =
  let length = string_length cs in
  let rec string_avoid_132_helper (i: int) (j: int) (k: int): bool =
    if i >= length then true
    else if j >= length then string_avoid_132_helper (i + 1) (i + 2) (i + 3)
    else if k >= length then string_avoid_132_helper i (j + 1) (j + 2)
    else if is_132_like (string_get_at cs i) (string_get_at cs j) (string_get_at cs k) then false
    else string_avoid_132_helper i j (k + 1)
  in
  string_avoid_132_helper 0 1 2
