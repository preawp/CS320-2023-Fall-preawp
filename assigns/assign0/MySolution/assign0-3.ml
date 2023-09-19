(* ****** ****** *)

#use "./../assign0.ml";;
(* ****** ****** *)
(*recursive helper function to count digit of a number*)
let rec int_len(i: int): int =
    if i < 0 then
    int_len(-i)
    else if i < 10 then
        1 
    else
        int_len(i / 10) + 1 
    
(*int2str function converts int to a string*)
let int2str(i0: int): string =
  (*get_digit returns the individual digit at specified index of number*)
  let rec get_digit(x: int): int =
    if x = 0 then 1 else 10 * get_digit (x-1)
  in

  let length = int_len (abs i0) in
  let is_neg = i0 < 0 in
  string_init (length + (if is_neg then 1 else 0))
  (fun i -> 
    if i = 0 && is_neg then '-'
    else
      (*dividing i0 to get different segments of the number and getting last digit*)
      let index = length - (if is_neg then (i - 1) else i) - 1 in
      let digit = abs (i0 / get_digit index) mod 10
    in
    chr(ord '0' + digit)
    );;
(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0-3.ml] *)
