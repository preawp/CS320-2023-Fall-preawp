#use "./../../../../classlib/OCaml/MyOCaml.ml";; 
#use "./../../assign3.ml";;
(*
Assign3-4:
HX-2023-09-26: 20 points
Given a word x of length n, another word is a buddy
of x if x and y differ exactly at one position. For
instance, "live" is a buddy "love" (and "love" is also
a buddy of "live").
//
Please give a NON-RECURSIVE implementation of
list_of_buddies that returns a list of all the buddies
of a given word.
//
let
list_of_buddies(word: string): string list = ...

(*
FYI. The concept of word buddies is used in the following game:
https://xanadu-lang.github.io/xats2js/docgen/CodeBook/Doublet/2020-11-29/
https://github.com/xanadu-lang/xats2js/tree/master/docgen/CodeBook/Doublet
*)
*)

(* ****** ****** *)

(*A string concatenation function*)
let string_concat_local (str1:string) (str2:string): string = 
  let string_list = [str1; str2] in
  string_concat_list string_list

  (* Custom string_sub_local function *)
let string_sub_local (s: string) (start: int) (length: int) : string =
  if length <= 0 || start < 0 || start >= string_length s then ""
  else
    let substring = ref "" in
    let end_pos =
      if (start + length) > (string_length s) then (start + length) 
      else string_length s in
    for i = start to end_pos - 1 do
      substring := string_append (!substring) (str(string_get_at s i));
    done;
    !substring

  
(*non-recursive list_of_buddies function*) 
let list_of_buddies(word: string): string list =
  let n = string_length word in
  let alphabet = "abcdefghijklmnopqrstuvwxyz" in
  let buddies = ref [] in
  for i = 0 to n - 1 do
    for j = 0 to 25 do
      if string_get_at alphabet j <> string_get_at word i then
        let left = string_sub_local word 0 i in
        let right = string_sub_local word (i + 1) (n - i - 1) in
        let buddy = string_concat_local left (string_cons (string_get_at alphabet j) right) in
        buddies := buddy :: !buddies;
      done
  done;
  !buddies
