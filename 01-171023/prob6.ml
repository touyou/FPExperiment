let rec fold_right f arr e =
  match arr with
  | [] -> e
  | x :: xs -> f x (fold_right f xs e);;
let rec fold_left f e arr =
  match  arr with
  | [] -> e
  | x :: xs -> fold_left f (f e x) xs;;

(* fold_left版append  *)
let appendl a b =
  fold_left (fun xs x -> x :: xs) b (fold_left (fun xs x -> x :: xs) [] a);;

(* fold_right版append *)
let appendr a b =
  fold_right (fun x xs -> x :: xs) a b;;

(* fold_left版filter *)
let filterl f arr =
  fold_left (fun e x -> if f x then x :: e else e) [] (fold_left (fun xs x -> x :: xs) [] arr);;

(* fold_right版filter *)
let filterr f arr =
  fold_right (fun x e -> if f x then x :: e else e) arr [];;
