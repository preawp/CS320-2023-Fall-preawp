#use "./../../../../classlib/OCaml/MyOCaml.ml";; 



(* ****** ****** *)

type 'a strcon =
  StrNil
| StrCons of
  'a * (unit -> 'a strcon)

(* ****** ****** *)

type 'a stream =
unit -> 'a strcon (* thunk *)

(* ****** ****** *)

let rec
stream_map
(fxs: 'a stream)
(fopr: 'a -> 'b): 'b stream =
fun () ->
match fxs() with
|
StrNil -> StrNil
|
StrCons(x1, fxs) ->
StrCons
(fopr(x1), stream_map(fxs)(fopr))
;;
(* ****** ****** *)

let rec
stream_foreach
(fxs: 'a stream)
(work: 'a -> unit): unit =
match fxs() with
| StrNil -> ()
| StrCons(x1, fxs) ->
  (work(x1); stream_foreach(fxs)(work))
;;
(* ****** ****** *)

let
int1_map_stream
(n0: int)
(fopr: int -> 'a): 'a stream =
let rec
helper(i: int) =
fun () ->
if i >= n0
then StrNil(*void*)
else StrCons(fopr(i), helper(i+1)) in helper(0)

(* ****** ****** *)

(* end of [CS320-2023-Fall-classlib-MyOCaml.ml] *)
(*
//
Assign4-1:
//
HX-2023-10-05: 10 points
//
The following is a well-known series:
ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...
Please implement a stream consisting of all the
partial sums of this series.
The 1st item in the stream equals 1
The 2nd item in the stream equals 1 - 1/2
The 3rd item in the stream equals 1 - 1/2 + 1/3
The 4th item in the stream equals 1 - 1/2 + 1/3 - 1/4
And so on, and so forth
//
let the_ln2_stream: float stream = fun() -> ...
//
*)
let the_ln2_stream: float stream =
  let rec partial_sums n sum =
    fun () ->
      let sign = if n mod 2 = 0 then -1.0 else 1.0 in
      let term = sign /. float_of_int (n + 1) in
      let new_sum = sum +. term in
      StrCons(new_sum, partial_sums (n + 1) new_sum)
  in
  partial_sums 0 1.0

val fxs = the_ln2_stream
