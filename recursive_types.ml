let rec rev1 l = match l with
  | [] -> []
  | x :: xs -> rev1 xs @ [ x ]

(*
  The list parameter i'm matching against is replaced by the shorthand notation
  'function'. It is the second param of the function def.
*)
let rec rev_aux1 accu = function
  (* when the remaining list is empty, i return the accumulator *)
  | [] -> accu
  (* 
    With each iteration, i put the first element of the remaining list
    at the top of the accumulator
  *)
  | x :: xs -> rev_aux1 ( x :: accu) xs

(* equivalent with: *)

let rec rev_aux accu l = match l with
  | [] -> accu
  | x :: xs -> rev_aux ( x :: accu) xs

(* 
  so the variable i'm matching against - l -,
  since i don't use it in the body of my computation, i can just replace it
  with the 'function' notation as a shorthand.
  https://stackoverflow.com/questions/33266050/the-difference-of-the-function-keyword-and-match-with-in-ocaml  
*)

(*
  here i use that function to compute the reverse of a list.
  the accumulator is an empty list initially 
*)
let rev2 l = rev_aux [] l ;;

(* ========= *)