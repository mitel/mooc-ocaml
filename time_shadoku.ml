type date =
  { year : int; month : int; day : int;
    hour : int; minute : int } ;;

let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 } ;;

(*
  A date is well-formed if 
    its year index is >= 1, 
    its month index is >= 1 and <= 5, 
    its day index is >= 1 and <= 4, 
    its hour index is >= 0 and <= 2, and 
    its minute index is >= 0 and <= 1. 

  a year has 5 months, 
  each month has 4 days, 
  each day has 3 hours and 
  each hour has 2 minutes
*)

let wellformed date = 
  if date.year >= 1 && 
     date.month >= 1 && date.month <= 5 && 
     date.day >= 1 && date.day <= 4 &&
     date.hour >= 0 && date.hour <= 2 &&
     date.minute >= 0 && date.minute <= 1 then true else false ;;

(*
  computes the date which comes one minute after the input date
*)
let next date =
  if date.minute < 1 then { 
    date with minute = date.minute + 1 
  } else
  if date.hour < 2  then { 
    date with minute = 0; 
              hour = date.hour + 1 
  } else
  if date.day < 4 then { 
    date with minute = 0;
              hour = 0; 
              day = date.day + 1 
  } else 
  if date.month < 5 then { 
    date with minute = 0;
              hour = 0; 
              day = 1;
              month = date.month + 1 
  } else { 
    minute = 0;
    hour = 0; 
    day = 1;
    month = 1;
    year = date.year + 1 
  }

(*
  In this computer, the time is represented by an integer 
  that counts the number of minutes since 1/1/1 0:0 
  (the origin of time). 
  Write a function of_int : int -> date that converts such
  an integer into a date.

  need a rec function that takes as parameter the previous result
*)
let of_int minutes = 
  let rec f count date =
    (if count = 1 then next date else
       let next_date = next date in
       f (count - 1) next_date) in 
  f minutes the_origin_of_time
