(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)
(*helper function to convert specific char to int*)
let char2int(c: char): int =
  (ord(c) - 48) 

(*recursive helper function to compute power of 10 *)
let rec power(i: int):int = 
  if i = 0 then
    1
  else 
    10 * power(i-1)
(*process from right to left with a recursive helper function, 
   generate value of the character at index i in the string and 
   adding it to returning value *)
let str2int(cs: string): int =
    let lastindex = string_length cs - 1 in
    let rec helper(i:int) : int =
      if i < 0 then
        0
      else 
        (char2int(string_get(cs, i)) * power(lastindex - i)) + helper(i-1)
    in 
    if cs = "" then
      0
    else 
      helper lastindex


    
(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0-4.ml] *)