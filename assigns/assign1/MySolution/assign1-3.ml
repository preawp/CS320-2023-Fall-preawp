#use "./../../../classlib/OCaml/MyOCaml.ml";; 
(*kinda*)
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

let
string_avoid_132
(cs: string): bool =
let len = string_length(cs) in

let rec
aux1(i: int): bool =
 int1_forall(len-i-1)(fun j -> aux2(i)(i+j+1))
and
aux2(i: int)(j: int): bool =
 int1_forall(len-j-1)(fun k -> aux3(i)(j)(j+k+1))
and
aux3(i: int)(j: int)(k: int): bool =
let c1 = string_get_at(cs)(i)
and c2 = string_get_at(cs)(j)
and c3 = string_get_at(cs)(k) in not((c1 < c3) && (c3 < c2))

in
int1_forall(len)(fun i -> aux1(i))
;; 