(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)
(*generate reversed string by fetching characters from 'cs' in reversed order and use string_init to create new string **)
let stringrev (cs: string): string = 
  let rev_index i = string_get(cs, string_length(cs) - i - 1) in
  string_init (string_length(cs)) rev_index

(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0-5.ml] *)