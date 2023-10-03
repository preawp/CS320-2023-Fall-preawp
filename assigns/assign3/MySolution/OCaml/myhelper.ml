

(*calculate sum of the list*)
let rec sum lst =
  match lst with
  | [] -> 0
  | h :: t -> h + sum t

(*calculate len of the list*)
let rec lenlist (list: 'a list) : int =
    match list with 
    | [] -> 0
    | h :: t -> 1 +lenlist t

(*appends of the lists*)
let rec appendlist (list1: 'a list)(list2: 'a list): 'a list =
    match list1 with 
    | [] -> list2
    | h :: t -> h :: appendlist t list2
    
    let
    list_map
    (xs: 'a list)(fopr: 'a -> 'b): 'b list =
    list_foldright(xs)([])(fun x0 r0 -> fopr(x0) :: r0)
    ;;
    let
    list_map
    (xs: 'a list)(fopr: 'a -> 'b): 'b list =
    list_make_fwork
    (
    fun work -> list_foreach(xs)(fun x -> work(fopr(x))))
    ;;
    (* ****** ****** *)

    let rec list_hd lst =
      match lst with
      | [] -> raise (Failure "No head")
      | x :: _ -> x
    ;;
    
    let rec list_tl lst =
      match lst with
      | [] -> raise (Failure "No tail")
      | _ :: x -> x
    ;;