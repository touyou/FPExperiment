(* permutationを実装する。特定の要素を消すためにfilterを応用する。
また全ての要素を一つずつ取り出して一つ一つで作っていきたいのでHaskellのconcatMapを自作する。 *)
let rec filter f arr =
    match arr with
    | [] -> []
    | x :: xs when f x -> x :: filter f xs
    | x :: xs -> filter f xs;;
let rec map f arr =
    match arr with
    | [] -> []
    | x :: xs -> f x :: map f xs;;
let rec concatMap f arr =
    match arr with
    | [] -> []
    | x :: xs -> (f x) @ concatMap f xs;;
let delete x arr =
    filter (fun y -> x != y) arr;;

let rec perm arr =
    match arr with
    | [] -> [[]]
    | xs -> concatMap (fun x -> map (fun y -> x :: y) (perm (delete x xs))) xs;;
