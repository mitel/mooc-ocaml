type color = Black | Gray | White ;;

let lighter c1 c2 =
  match (c1, c2) with
  | (Black, Black) -> false
  | (White, White) -> false
  | (Gray, Gray) -> false
  | (Black, _) -> true
  | (_, White) -> true
  | (White, Gray) -> false
  | (Gray, Black) -> false
  | (White, Black) -> false
;;

type index = Index of int ;;

(* 
  Write a function 
    read : int array -> index -> int
  such that read a (Index k) returns the k-th element of a.
*)

let read a index = 
  let rec search k = 
    if (Index k = index) then 
      a.(k) + 0
    else search (k+1) in 
  search 0 ;;

let read_ a index = 
  let rec search k = 
    match (Index k = index) with
    | true -> a.(k) + 0
    | false -> search (k+1)
  in search 0 ;;

let read a index = 
  match index with
  | Index k -> a.(k) + 0;;


(*
  Write a function 
    inside : int array -> index -> bool such that 'inside a idx' is true 
    if and only if idx is a valid index for the array a
*)
let inside a index =
  let rec search k = 
    if k < Array.length a then
      match (Index k = index) with
      | true -> let _el = a.(k) + 0 in true
      | false -> search (k+1)
    else false
  in search 0 ;;

let inside a index = 
  match index with
  | Index k -> if k >= 0 && k < Array.length a then true else false

(*
  Write a function 
    next : index -> index such that 
    next (Index k) is equal to Index (k + 1)
*)
let next index = match index with Index k -> Index (k + 1) ;;

(*
  Consider a non empty array of integers a, write a function 
    min_index : int array -> index
  that returns the index of the minimal element of a
*)

let v index = match index with Index k -> k ;;

let min_index a =
  let rec loop min idx min_idx= 
    if inside a idx then
      let current_el = read a idx in
      if current_el < min then 
        let new_min = current_el and 
        new_min_idx = idx in 
        loop new_min (next idx) new_min_idx
      else 
        loop min (next idx) min_idx
    else min_idx
  in loop (read a (Index 0)) (Index 1) (Index 0)

(*
  Write a function 
    find : string array -> string -> int option 
  such that 
    find a w = Some idx if a.(idx) = w and find a w = None if there is no such index
*)
let find a w = 
  let rec loop idx = 
    if idx = Array.length a then None else 
    if a.(idx) = w then Some idx else loop (idx + 1)
  in loop 0 

(*
  Sometimes, when a value of type t is missing, a default value should be used. 
  Write a function 
    default_int : int option -> int 
  such that: default_int None = 0 and default_int (Some x) = x
*)
let default_int x = match x with
  | None -> 0
  | Some x -> x;;

(*
  Write a function 
    merge : int option -> int option -> int option 
  such that:
    merge None None = None
    merge (Some x) None = merge None (Some x) = Some x
    merge (Some x) (Some y) = Some (x + y)
*)
let merge a b = match (a, b) with 
  | (None, None) -> None
  | (Some x, None) | (None, Some x) -> Some x
  | (Some x, Some y) -> Some (x + y)
