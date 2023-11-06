(* ****** ****** *)

#use "./../assign0.ml";;

(*Checked*)
(* ****** ****** *)

(*
Assign0-2: 10 points
Please implement a function that tests whether
a given natural number is a prime:
fun isPrime(n0: int): bool
*)

(* ****** ****** *)

let rec isPrime(n0: int): bool =
  if n0 < 2 then false 
  else if n0 = 2 || n0 = 3 then true
  else
   let rec helper(x: int):bool =
     if x >= n0 then true
     else if n0 mod x = 0 then false 
     else helper(x+1)
    in helper(2)