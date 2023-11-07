#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*
Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

*)

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accpeted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)


(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

*)

  let parse_digit (x:char) : expr option =
  let t = ord(x) - ord('0') in
  if (0 <= t) && (t <= 9) then Some(Int t) else None 

  let parse_num (s: char list) : (int * char list) option =
    match trim s with
    | d :: xs -> (
      match parse_digit d with
      | Some(Int number) -> Some (number, xs)
        | _ -> None
      )
    | _-> None


  let rec parse_expr(s: char list): (expr * char list) option =
    match trim s with
    | '('::'a'::'d'::'d'::' ':: xs -> (
      match parse_exprs trim xs with
      | Some (xss, ')' :: rest) -> Some (Add xss, rest)
      | _ -> None
    )
    | '('::'m'::'u'::'l'::' ':: xs -> (
      match parse_exprs trim xs with
      | Some (xss, ')' :: rest) -> Some (Mul xss, rest)
      | _ -> None
    
    ) 
    | xs -> (match parse_num xs with 
      | Some (n, xs') -> Some (Int n, xs')
      | _ -> None
    )

  
  and parse_exprs s =
    let rec helper acc cs= (
        match parse_expr cs with
        | Some(expr,rest) -> helper (list_append acc [expr]) (trim rest)
        | None when acc <> [] -> Some (acc, cs)
        | _ -> None 
      )
  in helper []
  
  let parse (s : string) : expr option = 
    let c = trim (string_listize(s)) in
    match parse_expr c with
    | Some(h,[]) -> Some h
    | _ -> None