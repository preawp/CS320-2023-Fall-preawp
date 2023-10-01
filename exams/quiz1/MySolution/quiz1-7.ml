#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* No other library imports are allowed. *)

(* ************************************************ *)

(* Question 7: 10 points

   Given the following snippet, implement the test
   function so that isPrime returns true for prime
   number inputs and false otherwise. *)

(* ****** ****** *)

let isPrime(n) =
  let test(i:int): bool =
    if n = 2 || n = 3 then true
    else
    let rec loop (i1: int):bool =
      if i1 * i1 > n then true
      else if n mod i1 = 0 then false
      else loop (i1 + 1)
    in
    loop 2            
  in
  if n < 2 then false else int1_forall(n)(test)

 
(* ************************************************ *)
