(* 再帰とパターンマッチングで一つずつ適用してつなげる  *)
let rec map f arr =
    match arr with
    | [] -> []
    | x :: xs -> f x :: map f xs;;
