(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

let rec expo(i: int): int =
  if i > 1 then 
    expo(i - 1) * 10
  else 
    1


let char2int(c: char, i: int): int =
    if i = 0 then
      (ord(c) - 48) 
    else
      (ord(c) - 48) * expo(i - 1)

let rec helper(cs: string, i: int): int =
    if i > 0 then
      char2int(string_get cs (string_length cs - i )) + helper(cs, i-1)
    else
      0
      
  
let str2int(cs: string): int =
      helper(cs, string_length(cs))
    
(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0-4.ml] *)