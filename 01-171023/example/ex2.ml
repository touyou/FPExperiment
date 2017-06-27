(* nをアキュミュレーターとして再帰関数で計算  *)
let rec sigma f n =
    match n with
    | 0 -> f 0
    | m -> f m + sigma f (m-1);;
