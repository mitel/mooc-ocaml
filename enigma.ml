(* Let us solve the following puzzle: 
   If you multiply my grand-son age by four, you know how old I am. Now, if you exchange the two digits of our ages then you have to multiply by three my age to get the age of my grand-son!

   Write a function exchange of type int -> int that takes an integer x between 10 and 99 and returns an integer which is x whose digits have been exchanged. For instance, exchange 73 = 37.
   Define is_valid_answer of type int * int -> bool such that is_valid_answer (grand_father_age, grand_son_age) returns true if and only if grand_father_age and grand_son_age verify the constraints of the puzzle.
   Write a function find : (int * int) -> (int * int) that takes a pair (max_grand_father_age, min_grand_son_age) and returns a solution (grand_father_age, grand_son_age) to the problem, where min_grand_son_age <= grand_son_age < grand_father_age <= max_grand_father_age or (-1, -1) if there was no valid answer in the given range. 
*)

let exchange k = if k >= 10 && k <= 99 then k mod 10 * 10 + k / 10 else k ;;

let exch_3 k = (exchange k) * 3 ;;

let is_valid_answer (grand_father_age, grand_son_age) = if (grand_father_age = grand_son_age * 4) && (exch_3 grand_father_age = exchange grand_son_age) then true else false;;

(* (x, y) = (max_grand_father_age, min_grand_son_age) *)
(* (grand_father_age, grand_son_age)*)

let find (max_grand_father_age, min_grand_son_age) = 
  (* 
    inner-functie recursiva 
    am nevoie de inner-functia asta ca sa salvez valorile
    din parametrii initiali max_.. si min_..
  *)
  let rec f (x, y) = 
    (* conditie de oprire finala cu insucces*)
    if y = max_grand_father_age-1 then (-1, -1) else
      (* conditie de oprire finala cu succes*)
    if is_valid_answer (x, y) then
      (x, y)
    else (
      (* cat timp grand_father age e mai mare, drecrementez 
         si re-execut functia, adica retestez  is_valid_answer*)
      if x>y then 
        f(x-1, y)
        (* cand x = y, incrementez y si o iau de la capat*)
      else f(max_grand_father_age, y+1)
    ) 
  in f (max_grand_father_age, min_grand_son_age)