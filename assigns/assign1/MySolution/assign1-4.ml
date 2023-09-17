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

(* Function to add two strings representing non-negative integers digit by digit *)
(* Function to add two strings representing non-negative integers digit by digit *)
let intrep_add(ds1: string)(ds2: string): string =
  let rec add_strings_with_carry ds1 ds2 carry result =
    match (ds1, ds2) with
    | ("", "") -> if carry > 0 then string_cons (char_of_digit carry) result else result
    | _ ->
        let digit1 = digit_of_char (char_of_string ds1.string_length ds1 - 1) in
        let digit2 = digit_of_char (char_of_string ds2.string_length ds2 - 1) in
        let (new_digit, new_carry) = add_digits digit1 digit2 carry in
        let new_result = string_cons (char_of_digit new_digit) (result) in
        add_strings_with_carry (string_sub ds1 0 (String.length ds1 - 1)) (string_sub ds2 0 (String.length ds2 - 1)) new_carry new_result
  in

  let result = add_strings_with_carry ds1 ds2 0 "" in
  if result = "" then "0" else result
;;

