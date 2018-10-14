type point  = { x : float; y : float; z : float } ;;
type dpoint = { dx : float; dy : float; dz : float } ;;
type physical_object = { position : point; velocity : dpoint } ;;

let move p dp = {
  x = p.x +. dp.dx; 
  y = p.y +. dp.dy; 
  z = p.z +. dp.dz
};;

let next obj = {
  position = move obj.position obj.velocity;
  velocity = obj.velocity
};;

(* 
  formula pt distanta dintre doua sfere 3D:
  http://www.gamasutra.com/view/feature/3015/pool_hall_lessons_fast_accurate_.php 
*)
let will_collide_soon p1 p2 =
  (* calculez urmatoarea pozitie *)
  let next_p1 = next p1
  and next_p2 = next p2 in
  (* distantele pe fiecare axa *)
  let dist_x = (next_p1.position.x -. next_p2.position.x) ** 2.
  and dist_y = (next_p1.position.y -. next_p2.position.y) ** 2.
  and dist_z = (next_p1.position.z -. next_p2.position.z) ** 2. in
  (* distanta dintre sfere *)
  let dist = sqrt (dist_x +. dist_y +. dist_z) in
  (* daca distanta e mai mica sau egala cu suma razelor *)
  if dist <= 2.0 then true else false;;
