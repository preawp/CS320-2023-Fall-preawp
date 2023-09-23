#use "./../../../../classlib/OCaml/MyOCaml.ml";; 
#use "./../../assign2.ml";;


(*
//
Assign2-3: 10 points
//
Please implement foldleft_to_iforeach that turns a
foldleft-loop into a iforeach-loop:
let
foldleft_to_iforeach
(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = ...
*)

(* ****** ****** *)
let rec
list_foldleft
(xs: 'a list)
(r0: 'r0)(fopr: 'r0 -> 'a -> 'r0): 'r0 =
match xs with
| [] -> r0
| (x1 :: xs) ->
  list_foldleft(xs)(fopr(r0)(x1))(fopr)
;;
let rec mylist_length(xs: 'a mylist): int = 
  match xs with 
  | MyNil -> 0 
  | MyCons(_, t) -> 1 + mylist_length t
  | MySnoc(t, _) -> 1 + mylist_length t
  | MyReverse(t) -> mylist_length t
  | MyAppend2(t1, t2) -> mylist_length t1 + mylist_length t2
  
let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  fun xs work ->
    let rec helper i r0 =
      match xs with
      | MyNil -> ()
      | MyCons(x1, rest) ->
        begin
          work i x1;
          helper (i + 1) (foldleft rest r0 (fun acc x -> acc))
        end
      | MySnoc(rest, x1) ->
        begin
          helper i (foldleft rest r0 (fun acc x -> acc));
          work (i + 1) x1
        end
      | MyReverse(rlst) -> helper i (foldleft rlst r0 (fun acc x -> acc))
      | MyAppend2(lst1, lst2) ->
        let len1 = mylist_length(lst1) in
        let r1 = foldleft lst1 r0 (fun acc x -> acc) in
        let r2 = foldleft lst2 r0 (fun acc x -> acc) in
        for i = 0 to len1 - 1 do
          work i (mylist_get_at(lst1) i);
        done;
        for i = len1 to len1 + mylist_length(lst2) - 1 do
          work i (mylist_get_at(lst2) (i - len1));
        done;
    in
    helper 0
;;


(* ****** ****** *)

