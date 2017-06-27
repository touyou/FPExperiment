let rec fold_right f arr e =
  match arr with
  | [] -> e
  | x :: xs -> f x (fold_right f xs e);;
let reverse arr = (fold_right (fun b g x -> g (b::x)) arr (fun x -> x)) [];;

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree;;

(* 行き掛け順 pre-order *)
let rec preacc a l =
  match a with
  | Leaf ->  l
  | Node (x, y, z) -> x :: preacc y (preacc z l);;

let preorder a = preacc a [];;

(* 通りがけ順 in-order *)
let rec inacc a l =
  match a with
  | Leaf -> l
  | Node (x, y, z) -> inacc y (x :: inacc z l);;

let inorder a = inacc a [];;

(* 帰りがけ順 post-order *)
let rec postacc a l =
  match a with
  | Leaf -> l
  | Node (x, y, z) -> postacc y (postacc z (x :: l));;

let postorder a = postacc a [];;
