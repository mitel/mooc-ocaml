(*
  A queue is a standard FIFO data structure. See wikipedia

  In this exercise, we implement a queue with a pair of two
  lists (front, back) such that front @ List.rev back
  represents the sequence of elements in the queue.

  So i'm implementing the queue with two stacks: front and back
    - when i want to enqueue, i need to push elements to 'back' 
      using the :: operator. 
      Virtually, because i look at 'back' in reverse order, it's like
      putting an element at the end of the 'back' list
    - when i dequeue, i need to pop from the 'front' list.
      But if i start from scratch, 'front' might be empty, hence
      i need to balance the content first - so i need to move some
      elements from 'back' to 'front' -> the split function
*)

type queue = int list * int list ;;

let rec sum = function
  | [] -> 0
  | hd :: tl -> hd + sum tl ;;

(* 
  Write a function is_empty : queue -> bool such that
  is_empty q is true if and only if q has no element.
*)
let is_empty = function
  | ([], []) -> true
  | (_, _) -> false ;;

(*
  Write a function enqueue : int -> queue -> queue such
  that enqueue x q is the queue as q except that x is at
  the end of the queue.
*)
let enqueue x (front, back) = (front, x :: back);;

(*
  Write a function split : int list -> int list * int list
  such that 
    split l = (front, back) where l = back @ List.rev front 
    and the length of back and front is 
      List.length l / 2 or 
      List.length l / 2 + 1

  eg:
    push 3, push 34, push 13, push 74 => [][74, 13, 34, 3]
    front = empty -> must split
    split [74, 13, 34, 3] = [3, 34][74, 13]
*)

let split l =
  let half = List.length l / 2 in
  (* 
    I have two accumulators: front and back.
    'back' is initialized with the initial list i want to split. 
    At each step of the recursive function i take the head and 
    i push it into 'front'; then the new 'back' becomes the tail.
  *)
  let rec f front back index = match back with
    | [] -> ([], [])
    | x :: xs when (index < half) ->
      let new_front = x :: front and
      new_back = xs and
      new_index = index + 1 in
      f new_front new_back new_index
    | _ :: _ -> (List.rev back, List.rev front)
  in f [] l 0 ;;

(*
  Write a function 
    dequeue : queue -> int * queue 
      such that
    dequeue q = (x, q') 
  where x is the front element of the queue q 
  and q' corresponds to remaining elements. 
  This function assumes that q is non empty.
*)

let rec dequeue = function 
  | (x :: xs, back) -> (x, (xs, back))
  | ([], back) -> dequeue (split back) ;;


