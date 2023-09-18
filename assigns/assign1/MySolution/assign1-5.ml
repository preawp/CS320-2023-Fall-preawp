
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


let string_longest_ascend (xs: string) : string =
  let n = string_length xs in
  let lis_length = Array.make n 1 in
  let prev_index = Array.make n (-1) in
  
  (* Traverse the string and compute the LIS *)
  for i = 1 to n - 1 do
    for j = 0 to i - 1 do
      if xs.[i] >= xs.[j] && lis_length.(i) < lis_length.(j) + 1 then begin
        lis_length.(i) <- lis_length.(j) + 1;
        prev_index.(i) <- j
      end
    done
  done;
  
  (* Find the index of the maximum length in lis_length *)
  let max_index = ref 0 in
  let max_length = ref lis_length.(0) in
  for i = 1 to n - 1 do
    if lis_length.(i) > !max_length then begin
      max_index := i;
      max_length := lis_length.(i);
    end
  done;
  
  (* Backtrack to find the leftmost longest ascending subsequence *)
  let rec backtrack index seq =
    if index < 0 then seq
    else
      let new_seq = xs.[index] :: seq in
      backtrack prev_index.(index) new_seq
  in
  
  let longest_sequence = backtrack !max_index [] in
  
  (* Convert the sequence to a string *)
  let result = String.of_seq (List.to_seq longest_sequence) in
  
  result
