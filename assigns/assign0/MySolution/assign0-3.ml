(* ****** ****** *)

#use "./../assign0.ml";;
(* ****** ****** *)
(*recursive helper function to count digit of a number*)
let rec int_len(i: int): int =
    if i < 0 then
    int_len(-i)
    else if i < 10 then
        1 
    else
        int_len(i / 10) + 1 
    
(*recursive helper function to extract individual digit*)
let rec digit i n =
    if i < 10 || n = 1 then
        i mod 10
    else
        digit (i/10) (n-1)

(*helper function to convert single digit to char*)
let int2char(i: int): char =
    chr(i + 48) 

(*calculated length of string once, use it to iteratively build a string by string_init *)
let int2str (i0: int) : string = 
    let length = int_len i0 in
    string_init (length) (fun j -> int2char(digit i0 (length - j )))
(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0-3.ml] *)
