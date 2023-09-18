
#use "./../../../classlib/OCaml/MyOCaml.ml";;
(*
assign1-5: 20 points

A sequence of chars is ascending if any char in
the sequence is less than or equal to the following
one (when the following one does exist).
Given a string cs, please implement a function
that find the longest ascending subsequence of [cs].
If there are more than one such sequences, the left
most one should be returned.

fun string_longest_ascend(xs: string): string

For instance, given "1324561111", the function
string_longest_ascend returns "13456"

For instance, given "1234561111", the function
string_longest_ascend returns "123456"

For instance, given "1234511111", the function
string_longest_ascend returns "111111".
*)


(* Custom string_sub function *)
let custom_string_sub (s: string) (start: int) (len: int) : string =
  let s_len = String.length s in
  if start >= 0 && start < s_len && len >= 0 && (start + len) <= s_len then
    String.init len (fun i -> String.get s (start + i))
  else
    ""

let string_longest_ascend(cs: string): string =
  let rec string_longest_ascend_helper (xs: string) (current_seq: string) (longest_seq: string): string =
    match xs with 
    | "" ->  
      if String.length current_seq > String.length longest_seq then current_seq
      else longest_seq
    | _ ->
      let c = String.get xs 0 in
      let rest = custom_string_sub xs 1 (String.length xs) in
      if (rest = "") || (c <= String.get rest 0) then
        let new_seq = current_seq ^ (String.make 1 c) in
        if (String.length new_seq) > (String.length longest_seq) then
          string_longest_ascend_helper rest new_seq new_seq
        else 
          string_longest_ascend_helper rest new_seq longest_seq
      else
        string_longest_ascend_helper rest (String.make 1 c) longest_seq
  in
  let result = string_longest_ascend_helper cs "" "" in

  if String.length cs = 0 || (String.length cs > 0 && String.get cs (String.length cs - 1) >= String.get result (String.length result - 1)) then
    result
  else
    result ^ (String.make 1 (String.get cs (String.length cs - 1)))
;;

(* Test cases *)
print_endline (string_longest_ascend "12345");;
