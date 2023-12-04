#use "./../../../classlib/OCaml/MyOCaml.ml";;
(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)
(* abstract syntax tree of interp1 *)
type const =
  | Int of int
  | Bool of bool
  | Unit
  | Sym of string
  | Closure of (string * ((string * const) list) * com list)

and com =
  | Push of const | Pop | Swap | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt
  | Condition of com list * com list 
  | Bind | Lookup
  | Fun of com list
  | Call | Return 
  
and coms = com list

(* ------------------------------------------------------------ *)

(* parsers for interp1 *)
let parse_nat = 
  let* n = natural << whitespaces in pure n

let parse_int =
  (let* n = parse_nat in pure (Int n)) <|>
  (keyword "-" >> let* n = parse_nat in pure (Int (-n)))

let parse_bool =
  (keyword "True" >> pure (Bool true)) <|>
  (keyword "False" >> pure (Bool false))

let parse_unit =
  keyword "Unit" >> pure Unit

let parse_const =
  parse_int <|>
  parse_bool <|>
  parse_unit <|>
  (let* fst_char = parse_char in
  let* rst_chars = many(parse_char_or_int) in
  pure(Sym (string_append (str fst_char) (string_of_list rst_chars))))


let rec parse_com () = 
  (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
  (keyword "Pop" >> pure Pop) <|>
  (keyword "Swap" >> pure Swap) <|>
  (keyword "Trace" >> pure Trace) <|>
  (keyword "Add" >> pure Add) <|>
  (keyword "Sub" >> pure Sub) <|>
  (keyword "Mul" >> pure Mul) <|>
  (keyword "Div" >> pure Div) <|>
  (keyword "And" >> pure And) <|>
  (keyword "Or" >> pure Or) <|>
  (keyword "Not" >> pure Not) <|>
  (keyword "Lt" >> pure Lt) <|>
  (keyword "Gt" >> pure Gt) <|>
  (let* _ = keyword "If" in
   let* c1 = many (parse_com () << keyword ";") in
   let* _ = keyword "Else" in
   let* c2 = many (parse_com () << keyword ";") in
   let* _ = keyword "End" in
   pure (Condition(c1, c2)))  <|>
  (keyword "Bind" >> pure Bind) <|>
  (keyword "Lookup" >> pure Lookup) <|>
  (let* _ = keyword "Fun" in
   let* c = many (parse_com () << keyword ";") in
   let* _ = keyword "End" in
   pure (Fun c)) <|>
  (keyword "Call" >> pure Call) <|>
  (keyword "Return" >> pure Return) 

let parse_coms = many (parse_com() << keyword ";")

(* ------------------------------------------------------------ *)

(* interpreter *)

type stack = const list
type trace = string list
type prog = coms
type var = (string * const) list
let rec str_of_nat (n : int) : string =
  let d = n mod 10 in 
  let n0 = n / 10 in
  let s = str (chr (d + ord '0')) in 
  if 0 < n0 then
    string_append (str_of_nat n0) s
  else s

let str_of_int (n : int) : string = 
  if n < 0 then
    string_append "-" (str_of_nat (-n))
  else str_of_nat n

let toString (c : const) : string =
  match c with
  | Int i -> str_of_int i
  | Bool true -> "True"
  | Bool false -> "False"
  | Unit -> "Unit"
  | Sym s-> s
  | Closure (n,_,_) -> string_concat_list ["Fun<";n;">"]

let rec eval (s : stack) (t : trace) (v : var) (p : prog) : trace =
  match p with
  (* termination state returns the trace *)
  | [] -> t
  | Push c :: p0 (* PushStack *) -> eval (c :: s) t v p0
  | Pop :: p0 ->
    (match s with
     | _ :: s0 (* PopStack *) -> eval s0 t v p0
     | []      (* PopError *) -> eval [] ("Panic" :: t) v [])
  | Swap :: p0 ->
    (match s with
    | i :: j :: s0 -> eval ( j :: i :: s0) t v p0
    | _ :: [] -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v []
    )
  | Trace :: p0 ->
    (match s with
     | c :: s0 (* TraceStack *) -> eval (Unit :: s0) (toString c :: t) v p0
     | []      (* TraceError *) -> eval [] ("Panic" :: t) v [])
  | Add :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* AddStack *)  -> eval (Int (i + j) :: s0) t v p0
     | _ :: _ :: s0         (* AddError1 *) -> eval [] ("Panic" :: t) v []
     | []                   (* AddError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []              (* AddError3 *) -> eval [] ("Panic" :: t) v [])
  | Sub :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* SubStack *)  -> eval (Int (i - j) :: s0) t v p0
     | _ :: _ :: s0         (* SubError1 *) -> eval [] ("Panic" :: t) v []
     | []                   (* SubError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []              (* SubError3 *) -> eval [] ("Panic" :: t) v [])
  | Mul :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* MulStack *)  -> eval (Int (i * j) :: s0) t v p0
     | _ :: _ :: s0         (* MulError1 *) -> eval [] ("Panic" :: t) v []
     | []                   (* MulError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []              (* MulError3 *) -> eval [] ("Panic" :: t) v [])
  | Div :: p0 ->
    (match s with
     | Int i :: Int 0 :: s0 (* DivError0 *) -> eval [] ("Panic" :: t) v []
     | Int i :: Int j :: s0 (* DivStack *)  -> eval (Int (i / j) :: s0) t v p0
     | _ :: _ :: s0         (* DivError1 *) -> eval [] ("Panic" :: t) v []
     | []                   (* DivError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []              (* DivError3 *) -> eval [] ("Panic" :: t) v [])
  | And :: p0 ->
    (match s with
     | Bool a :: Bool b :: s0 (* AndStack *)  -> eval (Bool (a && b) :: s0) t v p0
     | _ :: _ :: s0           (* AndError1 *) -> eval [] ("Panic" :: t) v []
     | []                     (* AndError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []                (* AndError3 *) -> eval [] ("Panic" :: t) v [])
  | Or :: p0 ->
    (match s with
     | Bool a :: Bool b :: s0 (* OrStack *)  -> eval (Bool (a || b) :: s0) t v p0
     | _ :: _ :: s0           (* OrError1 *) -> eval [] ("Panic" :: t) v []
     | []                     (* OrError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []                (* OrError3 *) -> eval [] ("Panic" :: t) v [])
  | Not :: p0 ->
    (match s with
     | Bool a :: s0 (* NotStack  *) -> eval (Bool (not a) :: s0) t v p0
     | _ :: s0      (* NotError1 *) -> eval [] ("Panic" :: t) v []
     | []           (* NotError2 *) -> eval [] ("Panic" :: t) v [])
  | Lt :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* LtStack *)  -> eval (Bool (i < j) :: s0) t v p0
     | _ :: _ :: s0         (* LtError1 *) -> eval [] ("Panic" :: t) v []
     | []                   (* LtError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []              (* LtError3 *) -> eval [] ("Panic" :: t) v [])
  | Gt :: p0 ->
    (match s with
     | Int i :: Int j :: s0 (* GtStack *)  -> eval (Bool (i > j) :: s0) t v p0
     | _ :: _ :: s0         (* GtError1 *) -> eval [] ("Panic" :: t) v []
     | []                   (* GtError2 *) -> eval [] ("Panic" :: t) v []
     | _ :: []              (* GtError3 *) -> eval [] ("Panic" :: t) v [])
  | Condition (c1, c2) :: p0 -> 
    (match s with
    | Bool a :: s0 -> if a then eval s0 t v (list_append c1 p0) else eval s0 t v (list_append c2 p0)
    | _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v []
    )
  | Bind :: p0 -> 
    (match s with
    | Sym x :: v0 :: s0 -> eval s0 t (((x), v0) :: v ) p0
    | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
    | _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v []
    )

 | Lookup :: p0 ->
      let rec helper cs a s00 = 
        (match cs with
        | [] -> eval [] t [] []
        | (n, value)::tl -> if n = a then eval (value::s00) t v p0 
                             else helper tl a s00) in
      (match s with
      | Sym a :: s0 -> helper v a s0 
      | [] -> eval [] ("Panic" :: t) [] []
      | _ :: s0 -> eval [] ("Panic" :: t) [] [])


  | Fun f :: p0 -> (
      match s with
      | Sym a :: s0 -> eval ((Closure(a, v, f)):: s0) t v p0
      | _ :: s0 -> eval [] ("Panic" :: t) v []
      | [] -> eval [] ("Panic" :: t) v []
  )
  | Call :: p0 -> 
    (match s with 
    | Closure (f, i, j) :: a :: s0 -> eval (a :: (Closure (f, v, p0) :: s0)) t ((f, Closure(f, i, j)) :: i) j
    | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
    | _ ::s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v []
    )
  | Return :: p0 -> 
    (match s with 
    | Closure (f, i, j) :: a :: s0 -> eval (a:: s0) t i j
    | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
    | _ ::s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v []
    )
(* ------------------------------------------------------------ *)

(* putting it all together [input -> parser -> eval -> output] *)

let interp (s : string) : string list option =
  match string_parse (whitespaces >> parse_coms) s with
  | Some (p, []) -> Some (eval [] [] [] p)
  | _ -> None

(* ------------------------------------------------------------ *)

(* interp from file *)

let read_file (fname : string) : string =
  let fp = open_in fname in
  let s = string_make_fwork (fun work ->
      try
        while true do
          work (input_char fp)
        done
      with _ -> ())
  in
  close_in fp; s

let interp_file (fname : string) : string list option =
  let src = read_file fname in
  interp src

  