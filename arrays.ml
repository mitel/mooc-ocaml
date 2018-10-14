(*
  Write a function min : int array -> int that returns
  the minimal element of a
*)
let min a =
  let rec f index vec min = 
    (if index = (Array.length a) then min else
     if a.(index) - min < 0 then
       let new_min = a.(index) in
       f (index + 1) vec new_min
     else f (index + 1) vec min) in
  f 0 a a.(0) ;;

let min_index a =
  let rec f index vec min min_index= 
    (if index = (Array.length a) then min_index else
     if a.(index) - min < 0 then
       (* memorez noul minim si noul index al minimului*)
       let new_min = a.(index) 
       and new_min_index = index in
       (* execut recursiv f cu noile valori *)
       f (index + 1) vec new_min new_min_index
       (* execut recursiv f cu vechile valori *)
     else f (index + 1) vec min min_index) in
  f 0 a a.(0) 0;;

(* Write a function is_sorted : string array -> bool which
   checks if the values of the input array are sorted in
   strictly increasing order, implying that its elements
   are unique (use String.compare). *)
let is_sorted a =
  if Array.length a = 0 || Array.length a = 1 then true else 
    let rec f index vec is_sorted = 
      (if index = (Array.length a - 1) then is_sorted else
         (* daca valorile sunt egale, pastrez flagul si merg
            mai departe pe vector
            e o repetitie si inca nu pot lua o decizie

            varianta din exercitiu imi zice totusi sa ies cu
            false dc am repetitii 
         *)
         (* if String.compare a.(index) a.(index + 1) = 0 then
            f (index + 1) vec is_sorted else *)
         (* daca a < b pun flagul pe true si merg mai departe*)
       if String.compare a.(index) a.(index + 1) < 0 then
         let new_is_sorted = true in
         f (index + 1) vec new_is_sorted
         (* daca am descoperit un punct in vector in care
            a > b, ies direct*)
       else false) in
    f 0 a false ;;

(*
  https://en.wikipedia.org/wiki/Binary_search_algorithm

  Given an array A of n elements with values or records 
      A0, A1, ..., An−1
  sorted such that A0 ≤ A1 ≤ ... ≤ An−1, and target value T, 
  the following subroutine uses binary search to find the 
  index of T in A.
*)
let find_ dict word =
  let rec f l r = 
    (
      (* If L > R, the search terminates as unsuccessful. *)
      if l > r then -1 else
        (*
          Set m (the position of the middle element) to the 
          floor of (L + R) / 2, which is the greatest integer
          less than or equal to (L + R) / 2.
        *)
        let m = (l + r) / 2 in
        (*If Am < T, set L to m + 1 and go to step 2*)
        if String.compare dict.(m) word < 0 then
          let new_l = m + 1 in
          f new_l r else 
          (*If Am > T, set R to m − 1 and go to step 2 *)
        if String.compare dict.(m) word > 0 then
          let new_r = m - 1 in
          f l new_r else 
          (* Am = T, the search is done; return m *)
          m 
    ) in
  (* Set L to 0 and R to n − 1 *)
  f 0 (Array.length dict - 1) ;;

(* optimizare: Hermann Bottenbruch *)
let find dict word =
  if Array.length dict = 0 then -1 else
    let rec f l r = 
      (
        (* 
        If L = R, the search the search is done.
          If AL = T, return L. Otherwise, the search terminates 
          as unsuccessful.
        *)
        if l = r then (if dict.(l) = word then l else -1) else 
        (*
          Set m (the position of the middle element) to the ceiling
          of (L + R) / 2, which is the least integer greater than
          or equal to (L + R) / 2.
        *)
          let l_ = float_of_int l and
          r_ = float_of_int r in
          let m = int_of_float (ceil ((l_ +. r_) /. 2.0)) in
          (* If Am > T, set R to m − 1 and go to step 2. *)
          if String.compare dict.(m) word > 0 then
            let new_r = m - 1 in
            f l new_r else 
            (* Set L to m and go to step 2 *)
            let new_l = m in
            f new_l r 
      ) in
    (* Set L to 0 and R to n − 1 *)
    f 0 (Array.length dict - 1) ;;