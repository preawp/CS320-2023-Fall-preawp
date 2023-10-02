

(*calculate len of the list*)
let rec lenlist (list: 'a list) : int =
    match list with 
    | [] -> 0
    | h :: t -> 1 +lenlist t