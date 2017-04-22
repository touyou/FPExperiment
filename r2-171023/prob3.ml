type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree;;
exception OrderError

let rec map f arr =
  match arr with
  | [] -> []
  | x :: xs -> f x :: map f xs

let rec filter f arr =
  match arr with
  | [] -> []
  | x :: xs when f x -> x :: filter f xs
  | x :: xs -> filter f xs;;

(* レベル順は幅優先と同じ順番 *)
(* 調べた所関数型ではキューの代わりに余再帰というものを使えば実現可能ということでそれを書いてみる *)


let rec walk n t =
  if n = 0 then []
  else match t with
    | [] -> []
    | Leaf :: xs -> walk (n - 1) xs
    | (Node (_, l, r)) :: xs -> l :: r :: walk (n + 1) xs;;

(* 一応エラー処理 *)
let extract t =
  match t with
  | Node (x, _, _) -> x
  | _ -> raise OrderError;;

let isNode t =
  match t with
  | Leaf -> false
  | Node (_, _, _) -> true;;

let levelorder t = map extract (filter isNode (
  ));;
