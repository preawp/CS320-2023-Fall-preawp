#use "./../../../../classlib/OCaml/MyOCaml.ml";; 
#use "./../../assign2.ml";;
(*
//
Assign2-4: 10 points
//
Please given a combinator-based implementation of
string_sepjoin_list:
let
string_sepjoin_list
(sep: string)(xs: string list): string = ...

For instance,
string_sepjoin_list(",")(["1","22","333"]) = "1,22,333"
For instance,
string_sepjoin_list(";;")(["11","22","33"]) = "11;;22;;33"
*)
let concat_strings str1 str2 =
  let len1 = string_length str1 in
  let len2 = string_length str2 in
  let combined_length = len1 + len2 in
  let result = string_init combined_length (fun i ->
    if i < len1 then string_get_at str1 i
    else string_get_at str2 (i - len1)
  ) in
  result

let string_sepjoin_list (sep: string) (xs: string list): string =
  let concat_with_sep acc x =
    if acc = "" then x else concat_strings (concat_strings acc sep) x
  in
  list_foldleft xs "" concat_with_sep
