type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp

let example =
  EAdd (EInt 1, EMul (EInt 2, EInt 3))

(* Write the expression 2 * 2 + 3 * 3 in a variable my_example *)
let my_example = EAdd (EMul (EInt 2, EInt 2), EMul (EInt 3, EInt 3));;

(*
  Write a function 
    eval : exp -> int 
  that computes the value of an arithmetic expression. 
  The evaluation rules are:
    - If the expression is an integer x, the evaluation is x.
    - If the expression is 
        lhs + rhs and 
        lhs evaluates to x and 
        rhs evaluates to y, 
      then the evaluation is x + y.
    - If the expression is 
        lhs * rhs and 
        lhs evaluates to x and 
        rhs evaluates to y, 
      then the evaluation is x * y.
*)
let rec eval = function
  | EInt e -> e
  | EAdd (lhs, rhs) -> (
      match lhs, rhs with 
      | EInt x, EInt y -> x + y
      | _, _ -> eval lhs + eval rhs
    )
  | EMul (lhs, rhs) -> (
      match lhs, rhs with 
      | EInt x, EInt y -> x * y
      | _, _ -> eval lhs * eval rhs
    ) ;;

(*
  If an expression is of the form 
    a * b + a * c then a * (b + c) is a factorized equivalent expression. 

  Write a function 
    factorize : exp -> exp 
  that implements this transformation on its input exp
  if it has the shape a * b + a * c or does nothing otherwise.
*)
let factorize e = match e with
  | EAdd(lhs, rhs) -> (
      match lhs with 
      | EMul (a, b) -> (
          match rhs with 
          | EMul (a', c) when a' = a -> EMul(a, EAdd(b, c))
          | _ -> e
        )
      | _ -> e
    )
  | _ -> e ;;

(*
  Write the reverse transformation of factorize, 
    expand : exp -> exp, 
  which turns an expression of the shape a * (b + c) into a * b + a * c
*)
let expand e = match e with
  | EMul(a, rhs) -> (
      match rhs with
      | EAdd(b, c) -> EAdd(EMul(a, b), EMul(a, c))
      | _ -> e
    )
  | _ -> e ;;

(*
  Implement a function 
    simplify: exp -> exp 
  which takes an expression e and:
    - If e is of the shape e * 0 or 0 * e, returns the expression 0.
    - If e is of the shape e * 1 or 1 * e, returns the expression e.
    - If e is of the shape e + 0 or 0 + e, returns the expression e.
  and does nothing otherwise.
*)
let simplify exp = match exp with
  | EMul(e, EInt 0) | EMul(EInt 0, e) -> EInt 0
  | EMul(e, EInt 1) | EMul(EInt 1, e) -> e
  | EAdd(e, EInt 0) | EAdd(EInt 0, e) -> e
  | _ -> exp ;;


