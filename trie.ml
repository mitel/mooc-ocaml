(*
  2 mutually defined types:
    - trie which represents a trie, that is a tree whose root may
      contain an integer and whose children are indexed by characters ;
    - char_to_children which implements the associative data structure
      whose keys are characters and whose values are trie (childrens).
*)
type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

let empty =
  Trie (None, [])

let example =
  Trie (None,
        [('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
         ('t',
          Trie (None,
                [('e',
                  Trie (None,
                        [('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
                         ('a', Trie (Some 3, []))]));
                 ('o', Trie (Some 7, []))]));
         ('A', Trie (Some 15, []))])
(*
  Write a function 
    children_from_char : char_to_children -> char -> trie option 
  such that
    - children_from_char m c = Some t if (c, t) 
      is the first pair in m with c as a first component ;
    - children_from_char m c = None if no such pair exists in m

  So basically i'm implementing a search function on a Trie, where
  the input is a certain character and the output is the first sub-trie
  i can find - so it will search only the first level of the trie 
  corresoponding to the first possible letter.
*)

let rec children_from_char (m:char_to_children) (c:char) = match m with
  | (c', t) :: tl -> (
      match t with 
      | Trie _ when c' = c -> Some t
      | Trie _ when c' <> c -> children_from_char tl c
      | _ -> None
    ) 
  | _ -> None ;;

(*
  Write a function 
    update_children : char_to_children -> char -> trie -> char_to_children 
  such that
    1. children_from_char (update_children m c t) c = Some t ;
    2. children_from_char (update_children m c t) c' 
        = children_from_char m c' for c <> c';
    3. If children_from_char m c = Some t 
        then List.length (update_children m c t') = List.length m
*)

let update_children m c t =
  if children_from_char m c = Some t then m else (c, t) :: m ;;

(*
  Write a function 
    lookup : trie -> string -> int option 
  such that 
    lookup trie w = Some i 
      if i is the value of the key w in trie and 
      lookup trie w = None if w is not a key of trie. 
  To look for a key in a trie, iterate over the characters of the
  key from left to right. 
  Given the current character c and the current node of the trie n,
  look for the children n for character c. 
  If such a children exists, continue with that trie and the remainder
  of the key. If no such children exists, the key is not in the trie.
  When the characters of the key are entirely consumed, look at the
  root of the current trie. If there is an integer, this is the value
  you are looking for. If there is no integer, the key not in the trie.
*)

