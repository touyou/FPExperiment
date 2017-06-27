type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree;;


let rec rev arr acc =
  match arr with
  | [] -> acc
  | x :: xs -> rev xs (x :: acc);;

(* 再帰バージョン *)
let reverse arr = rev arr [];;

let rec levelacc que acc =
  match que with
  | [] -> acc
  | tr :: xs ->
    match tr with
      | Leaf -> levelacc xs acc
      | Node (x, l, r) -> levelacc (xs @ (l :: r :: [])) (x :: acc);;

let levelorder tr = reverse (levelacc (tr :: []) []);;
