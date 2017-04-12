(* appendは片方から一つずつとりだしてコンスでくっつける。  *)

let rec rev arr acc =
    match arr with
    | [] -> acc
    | x :: xs -> rev xs (x :: acc);;


let rec appr a b acc =
    match b with
    | [] -> rev a acc
    | x :: xs -> appr a xs (x :: acc);;

let append a b =
    appr (rev a []) (rev b []) [];;


(* filterは１つずつ取り出してから追加していく。中にif文を入れても良いが今回はパターンマッチの条件を利用してみる。 *)

let rec filter f arr =
    match arr with
    | [] -> []
    | x :: xs when f x -> x :: filter f xs
    | x :: xs -> filter f xs;;
