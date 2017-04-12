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
    fold_left (fun ) (fold_left (fun x xs -> x :: xs) [] a) b
