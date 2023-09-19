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
    
(*main function converts int to a string*)
let int2str(i0: int): string =
  (* Helper function to retrieve a specific digit at a given position *)
  let rec get_digit(x: int): int =
    if x = 0 then 1 else 10 * get_digit (x-1)
  in

  let length = int_len (abs i0) in
  let is_negative = i0 < 0 in
  string_init (length + (if is_negative then 1 else 0))
  (fun index -> 
    if index = 0 && is_negative then '-'
    else
      let position = length - (if is_negative then (index - 1) else index) - 1 in
      let digit = abs (i0 / get_digit position) mod 10
    in
    (* Convert the digit to a character and add it to the result string *)
    chr(ord '0' + digit)
  )

(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0-3.ml] *)
