(*
  Write a function 
    mem : int -> int list -> bool 
  such that mem x l is true if and only if x occurs in l
*)

let rec mem x l = match l with 
  | [] -> false
  | e :: es when (e = x) -> true
  | _ :: es -> mem x es ;; 

(*
  Write a function 
    append : int list -> int list -> int list 
  such that 
    append l1 l2 is the concatenation of l1 and l2
*)
let append_ l1 l2 =
  (* needed by rev *)
  let rec rev_aux accu = function
    | [] -> accu
    | x :: xs -> rev_aux ( x :: accu) xs in 
  (* reversing a list *)
  let rev lst = rev_aux [] lst in
  (* reversing l1 *)
  let l1' = rev l1 in
  let rec f l acc = match l with 
    | [] -> acc
    | x :: xs -> f xs ( x :: acc)
  in f l1' l2 ;;

let rec append l1 l2 = match l1 with
  | [] -> l2
  | x :: xs -> x :: append xs l2 ;;

(*
  Write a function 
    combine : int list -> int list -> (int * int) list 
  such that 
    combine l1 l2 is the list of pairs obtained by joining
    the elements of l1 and l2. 
  This function assumes that l1 and l2 have the same 
  length. 
  For instance, combine [1;2] [3;4] = [(1, 3); (2, 4)]
*)
let rec combine l1 l2 = match l1, l2 with
  | x :: xs, y :: ys -> [(x, y)] @ (combine xs ys)
  | [], _  | _, [] -> []

(*
  Write a function 
    assoc : (string * int) list -> string -> int option 
  such that 
    assoc l k = Some x if (k, x) is the first pair of l 
    whose first component is k. 
    If no such pair exists, assoc l k = None

  assoc [("blah", 5); ("gogu", 3)] "gogu" = 3
*)
let rec assoc l k = match l with
  | [] -> None
  | (str, no) :: xs when (String.equal str k) -> Some (no + 0)
  | (_, _) :: xs -> assoc xs k ;;