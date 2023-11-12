#use "./../../../../classlib/OCaml/MyOCaml.ml"

(* ****** ****** *)
(*
//
Assign6:
Parsing and parsing combinators
//
DUE: the 13th of November, 2023
//
Except for the basic arithmetic functions
(including those on chars), you may only use
the functions in classlib/OCaml/MyOCaml.ml
//
*)
(* ****** ****** *)

(*
//
Assign6-1:
//
Please implement a print and parse function. Using parser combinators. When
given a valid string according to the grammar, your parse function returns an
sexpr value encoding the expression.

//
let sexpr_to_string (e : sexpr)  : string       = ...
let sexpr_parse     (s : string) : sexpr option = ...
//

Example (Accepted Strings):
sexpr_parse "(add 1 2 3)" = Some (SAdd [SInt 1; SInt 2; Int 3])
sexpr_parse "(mul (add 1 2) 3 (mul 1))" = Some (SMul [SAdd [SInt 1; SInt 2]; SInt 3; SMul [SInt 1]])
//
Example (Rejected Strings):
sexpr_parse "()" = None
sexpr_parse "(add)" = None
sexpr_parse "(add 1 2))" = None
sexpr_parse "((mul 1 2)" = None
//
*)

(* ****** ****** *)

(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<exprs> ::= <expr> | <expr> <exprs>
<expr>  ::= <num>
          | (add <exprs> )
          | (mul <exprs> )
*)

type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)

(* ****** ****** *)
let rec parse_expr () =
  parse_int () <|> parse_add () <|> parse_mul () 

and parse_int ()  = 
  let* x = natural in
  let* _ = whitespaces in
   pure(SInt x)

and parse_add () =
  let* _ = keyword "(add" in
  let* x = many1' parse_expr in 
  let* _ = keyword ")" in
  let* _ = whitespaces in
  pure (SAdd x)

and parse_mul () =
  let* _ = keyword "(mul" in
  let* x = many1' parse_expr in 
  let* _ = keyword ")" in
  let* _ = whitespaces in
  pure (SMul x)

let rec sexpr_to_string (e : sexpr) : string =
  match e with
  | SInt x -> string_of_int x
  | SAdd x | SMul x ->
    let syntax =
      match e with
      | SAdd _ -> "(add"
      | SMul _ -> "(mul"
      | _ -> ""
    in
    let expr =
      list_foldleft x "" (fun acc cs -> string_append acc (string_append " " (sexpr_to_string cs))) in
      string_append syntax (string_append expr ")")

let sexpr_parse (s : string) : sexpr option = 
  match string_parse (parse_expr()) s with
  | Some (expr, []) -> Some expr
  | _ -> None

(* end of [CS320-2023-Fall-assigns-assign6.ml] *)
