(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

let rec int2str(i0: int): string =
    if i0 < 10
        then str(chr(i0+48))
    else
      let lastdigit = i0 mod 10 in
      let remaining = i0 / 10 in
      int2str remaining ^ str(chr(lastdigit + 48))

(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0-3.ml] *)
