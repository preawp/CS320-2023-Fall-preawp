#use "./../../../classlib/OCaml/MyOCaml.ml";; 

(*
assign1-4: 20 points
Given two strings ds1 and ds2 of digits, please
implement intrep_add(ds1)(ds2) that returns the
string representing the sum of the two integers
represented by ds1 and ds2. Note that the returned
string should contain no leading zeros. (And we
use the empty string to represent the number zero).

fun
intrep_add(ds1: string)(ds2: string): string

For instance, intrep_add("1116123", "222987") = "3337110"

Note that ds1 and ds2 can be arbitrarily long. Thus,
converting ds1 and ds2 to integers can cause overflow.
*)

(*function from lecture to reverse a string*)
let
stringrev(cs:string): string =
string_make_fwork(string_rforeach(cs))

(*function to choose max length of two strings*)
let choose_max_len (str1: string)(str2: string): int =
  let len1 = string_length str1 in
  let len2 = string_length str2 in
  if len1 >= len2 then len1
  else len2

(*function from lecture to convert int2str*)
  let
  int2str(n0: int): string =
  let rec
  foreach(n0: int)
  (work: char -> unit) =
  if
  n0 >= 10
  then
  let
  d0 =
  char_of_digit(n0 mod 10) in
  (work(d0); foreach(n0/10)(work))
  else work(char_of_digit(n0))
  in
    string_rmake_fwork(foreach(n0))

(*A string concatenation function*)
let string_concat_local (str1:string) (str2:string): string = 
    let string_list = [str1; str2] in
    string_concat_list string_list

let intrep_add (ds1: string) (ds2: string): string =

  let num1 = stringrev ds1 in
  let num2 = stringrev ds2 in
  let len1 = string_length num1 in
  let len2 = string_length num2 in

  let max_len = choose_max_len ds1 ds2 in

  let rec intrep_add_helper (i: int) (carry: int) (acc:string): string =
    if i >= max_len then
      if carry > 0 then
        string_concat_local (int2str carry) acc
      else
        acc
    else
      let digit1 = if i < len1 then ord(string_get_at num1 i) - ord ('0') else 0 in
      let digit2 = if i < len2 then ord(string_get_at num2 i) - ord('0') else 0 in
      let sum = digit1 + digit2 + carry in
      let new_carry = sum / 10 in
      let new_digit = sum mod 10 in
      intrep_add_helper (i + 1) new_carry (string_concat_local (int2str new_digit) acc)
  in

  let result = intrep_add_helper 0 0 "" in

  result
;;