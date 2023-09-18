
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
let string_longest_ascend xs =
  let rec find_longest xs current longest =
    match xs with
    | "" -> longest
    | s ->
      let rec find_ascend s acc =
        match s with
        | "" -> acc
        | c :: rest ->
          let current_num = int_of_char c in
          match acc with
          | [] -> find_ascend rest [current_num]
          | hd :: _ when current_num <= hd -> acc
          | hd :: tl -> find_ascend rest (current_num :: acc)
      in
      let ascending = find_ascend s [] in
      if List.length ascending > List.length longest then
        find_longest (String.sub s (List.length ascending) (String.length s - List.length ascending)) [] ascending
      else
        find_longest (String.sub s 1 (String.length s - 1)) [] longest
  in
  find_longest xs [] []
