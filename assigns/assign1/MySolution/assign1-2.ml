#use "./../../../classlib/OCaml/MyOCaml.ml";; 

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
let string_merge(cs1: string)(cs2: string): string =
  let merged_string =
    string_make_fwork (fun work ->
      let len1 = string_length cs1 in
      let len2 = string_length cs2 in
      let rec merge_chars i1 i2 =
        if i1 < len1 || i2 < len2 then (
          let char1 = if i1 < len1 then string_get_at cs1 i1 else '\x7F' in
          let char2 = if i2 < len2 then string_get_at cs2 i2 else '\x7F' in
          if char1 <= char2 then (
            work char1;
            merge_chars (i1 + 1) i2
          ) else (
            work char2;
            merge_chars i1 (i2 + 1)
          )
        )
      in
      merge_chars 0 0
    )
  in
  merged_string
;;