#use "./../../classlib/OCaml/MyOcaml.ml";; 

(*
assign1-2: 10 points
Given two ordered strings cs1 and cs2, please use
string_make_fwork to implement string_merge(cs1, cs2)
which returns the order string obtained from merging
cs1 and cs2.

For instance, if cs1 = "135" and cs2 = "2468", then
string_merge(cs1)(cs2) equals "1234568"

For instance, if cs1 = "abcde" and cs2 = "1234", then
string_merge(cs1)(cs2) equals "1234abcde"
*)

(*recursive helper function to find smallest char*)
let rec find_min_char (cs: string) (min_char: char): char =
  if cs = "" then min_char
  else 
    let current_char = string_get_at cs 0 in
    let rest_chars = string_make_fwork(fun work -> string_foreach cs (fun c -> if c <> current_char then work c)) in
    let new_min_char = 
      if current_char < min_char then current_char
      else min_char in
      find_min_char rest_chars new_min_char 

(*recursive helper function to find reorder string*)
let rec string_reorder (cs: string) (acc: string): string =
  if string_length cs = 0 then
    acc
  else
    let min_char = find_min_char cs 'z' in
    let rest_chars = string_make_fwork (fun work -> string_foreach cs (fun c -> if c <> min_char then work c)) in
    let new_acc = string_snoc acc min_char in 
    string_reorder rest_chars new_acc 

(*main function that combine two string and call reorder function to return correct result*)
let string_merge(cs1: string) (cs2: string): string =
  let unordered_string = string_make_fwork(fun work -> string_foreach cs1 work; string_foreach cs2 work) in
  string_reorder unordered_string "" 
