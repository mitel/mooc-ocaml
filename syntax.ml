(*

match value with
| pattern  [ when condition ] ->  result
| pattern  [ when condition ] ->  result
  ...

*)

(* count the number of positive integers in a list *)
let rec count = function
  | [] -> 0
  | x :: rst when x > 0 -> 1 + count rst
  | _ :: rst -> 0 + count rst ;; 

let rec combine l1 l2 = match l1, l2 with
  | x :: xs, y :: ys -> [(x, y)] @ (combine xs ys)
  | [], _  | _, [] -> []