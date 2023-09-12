(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

let isPrime(n0: int): bool =
  if n0 <= 1 then 
    false
  else if n0 = 2 || n0 = 3 then
    true
  else
    let rec is_divisible i =
      if i > n0 / 2 then 
        true
      else if n0 mod i = 0 then
        false 
      else 
        is_divisible (i + 1)
    
    in is_divisible 2

(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0-2.ml] *)
