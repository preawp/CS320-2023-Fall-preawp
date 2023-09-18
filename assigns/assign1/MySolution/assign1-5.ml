
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
  let s_len = string_length s in
  if start >= 0 && start < s_len && len >= 0 && (start + len) <= s_len then
    string_init len (fun i -> string_get_at s (start + i))
  else
    ""
let string_longest_ascend s =
  let n = String.length s in
  let lis_length = Array.make n 1 in
  let prev_index = Array.make n (-1) in
  
  for i = 1 to n - 1 do
    for j = 0 to i - 1 do
      if s.[i] >= s.[j] && lis_length.(i) < lis_length.(j) + 1 then begin
        lis_length.(i) <- lis_length.(j) + 1;
        prev_index.(i) <- j
      end
    done;
  done;
  
  let max_length = Array.fold_left max 0 lis_length in
  let max_index = ref (-1) in
  for i = 0 to n - 1 do
    if lis_length.(i) = max_length then
      max_index := i
  done;
  
  let longest_sequence = ref [] in
  let index = ref !max_index in
  while !index >= 0 do
    longest_sequence := (String.make 1 s.[!index]) :: !longest_sequence;
    index := prev_index.(!index);
  done;
  
  String.concat "" (List.rev !longest_sequence)
;;
