#use "./../../classlib/OCaml/MyOCaml.ml";; 

(*
assign1-1: 10 points
Given a natural number n that is not a multiple
of 10, intrev10(n) is the natural number whose
representation in base 10 reverses that of the
number n.

fun intrev10(n: int): int

For instance, if n = 12345, then intrev10(n) is
54321; if n = 10203, then intrev10(n) is 30201.

Please give a TAIL-RECURSIVE implementation of
intrev10.
*)

(*Using a recursive helper function to extract the last-digit 
repeatedly and constructing the reversed integer through tail-recursive.*)

let intrev10(n: int): int =
   let rec intrev10_helper (n: int) (rest: int): int =
       if n = 0 then rest
         else 
          let last_digit = n mod 10 in
          let new_n = n / 10 in
          let cur_rest = last_digit + (rest * 10) in
          intrev10_helper new_n cur_rest 
  in intrev10_helper n 0 
    