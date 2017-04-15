let rec fold_right f arr e =
  match arr with
  | [] -> e
  | x :: xs -> f x (fold_right f xs e);;
let rec rev arr acc =
  match arr with
  | [] -> acc
  | x :: xs -> rev xs (x :: acc);;

(* 再帰バージョン *)
let reverse arr = rev arr [];;

(* fold_leftは簡単なのでfold_rightでfold_leftを実現してそれを用いて行う *)
(* let reversef arr = fold_right (fun x e -> e @ [x]) arr [];; *)
