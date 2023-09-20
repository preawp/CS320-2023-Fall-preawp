(* ************************************************ *)

(*

 Question 8: 20 points
 Please give a NON-RECURSIVE implementation of sort5
 that takes 5 integers and returns a tuple that consists
 exactly of the 5 given integers ordered increasingly

 For instance, sort5(1, 2, 1, 2, 1) = (1, 1, 1, 2, 2)
 For instance, sort5(1, 3, 4, 5, 2) = (1, 2, 3, 4, 5)
 For instance, sort5(1, 3, 5, 4, 2) = (1, 2, 3, 4, 5)

 You can implement your own helper functions as long as
 you do not make use of recursion.

*)

let sort5: int*int*int*int*int -> int*int*int*int*int =
  fun (x1, x2, x3, x4, x5) ->
    let compare(a, b) =
      if a <= b then (a, b)
      else (b, a)
    in

    let (x1, x2) = compare (x1,x2) in
    let (x2, x3) = compare (x2,x3) in
    let (x3, x4) = compare (x3,x4) in
    let (x4, x5) = compare (x4,x5) in

    let (x1, x2) = compare (x1,x2) in
    let (x2, x3) = compare (x2,x3) in
    let (x3, x4) = compare (x3,x4) in

    let (x1, x2) = compare (x1,x2) in
    let (x2, x3) = compare (x2,x3) in

    let (x1, x2) = compare (x1,x2) in

    (x1, x2, x3, x4, x5)

(* ************************************************ *)
