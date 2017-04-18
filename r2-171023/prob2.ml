type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree;;

(* 行き掛け順 pre-order *)
let rec preacc a l =
let pre-order = preacc a [];;
(* 通りがけ順 in-order *)
let in-order;;
(* 帰りがけ順 post-order *)
let post-order;;
