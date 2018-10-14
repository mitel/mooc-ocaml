(* A phone number is a touple of four integers. *)
type phone_number = int * int * int * int;;

(* A contact has a name and a phone number. *)
type contact = {
  name         : string;
  phone_number : phone_number
};;

(* Here is a dumb contact. *)
let nobody = { name = ""; phone_number = (0, 0, 0, 0) };;

(* A database is a collection of contacts. *)
type database = {
  number_of_contacts : int;
  contacts : contact array;
};;

(* [make n] is the database with no contact and at most [n] contacts
    stored inside. *)
let make max_number_of_contacts =
  {
    number_of_contacts = 0;
    contacts = Array.make max_number_of_contacts nobody
  };;

(* Queries are represented by a code and a contact.
   - If the code is 0 then the contact must be inserted.
   - If the code is 1 then the contact must be deleted.
   - If the code is 2 then we are looking for a contact
     with the same name in the database. *)
type query = {
  code    : int;
  contact : contact;
}

(*
  Has an error that leads to not finding users that should be in the
  database after certain sequences of queries. 
*)
let search db contact =
  let rec aux idx =
    (*
      AICI e bugul - ar tb sa fie idx >= Array.length db.contacts
      dar in schimb, cautarea presupune ca elementele sunt concentrate
      in primele n elemente din array
    *)
    if idx >= db.number_of_contacts then
      (false, db, nobody)
    else if db.contacts.(idx).name = contact.name then
      (true, db, db.contacts.(idx))
    else
      aux (idx + 1)
  in
  aux 0;;

let insert db contact =
  if db.number_of_contacts >= Array.length db.contacts then
    (false, db, nobody)
  else
    let (status, db, _) = search db contact in
    if status then (false, db, contact) else
      let cells i =
        if i = db.number_of_contacts then contact else db.contacts.(i)
      in
      let db' = {
        number_of_contacts = db.number_of_contacts + 1;
        contacts = Array.init (Array.length db.contacts) cells
      }
      in
      (true, db', contact);;

(*
  tb sa fac o versiune noua ca atunci cand sterg elemente
  cele ramase sa fie pe primele pozitii din array.
  deci sa nu am [nobody, blah, nobody, blah] ci 
  [blah, blah]
*)
let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let cells i =
      (* aici are loc stergerea
         daca pe pozitia i gasesc numele contactului pe care
         vreau sa-l scot, pun 'nobody'
      *)
      if db.contacts.(i).name = contact.name then
        nobody
      else
        db.contacts.(i) in
    (* o functie de sortare pt database *)
    let sort_f c1 c2 = 
      if c1.name < c2.name then -1 else if c1.name > c2.name then 1 else 0 in
    let db' = {
      number_of_contacts = db.number_of_contacts - 1;
      (* aplica functia 'cells' pe fiecare element din noul array
         initializat cu Array.init

         api ca sa aduc toate elementele diferite de 'nobody'
         la inceputul array-ului, sortez si fac reverse. 
      *)
      contacts = 
        let new_contacts = Array.init (Array.length db.contacts) cells in
        let _ = Array.sort sort_f new_contacts in
        let new_contacts_list = Array.to_list new_contacts in
        let reverse_list = List.rev new_contacts_list in
        Array.of_list reverse_list
    }
    in
    (true, db', contact);;

(* Engine parses and interprets the query. *)
let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else (false, db, nobody);;

let proof_of_bug =
  [| 
    {code= 0; contact=  {name = "Yannto0"; phone_number = (4, 8, 6, 10)}}; 
    {code= 0; contact=  {name = "Yannto1"; phone_number = (4, 8, 6, 10)}};
    {code= 0; contact=  {name = "Yannto2"; phone_number = (4, 8, 6, 10)}};
    {code= 1; contact=  {name = "Yannto1"; phone_number = (4, 8, 6, 10)}};
    {code= 2; contact=  {name = "Yannto2"; phone_number = (4, 8, 6, 10)}}
  |] ;;

(* 
  Write a new function 
    update : database -> contact -> (bool * database * contact)
  that either changes the number of an existing person or inserts
  a new contact. It should return true and the updated database 
  if any of these two options succeeded, or false with the untouched database.
*)

let update db contact =
  (* caut contactul, daca nu il gasesc, il inserez*)
  let (status, db, existing_contact) = search db contact in
  (* if found, i modify the contact number *)
  if status then
    let (s', d', _) = delete db existing_contact in
    insert d' contact
  else
    insert db contact ;;

(*
  Write an updated engine function that does an update when
  given a query with code 3, and uses your updated delete function
*)
let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else if code = 3 then update db contact
  else (false, db, nobody);;