(* 二回の合成。  *)

let twice f x = f (f x);;

(* n回の合成。再帰。*)

let rec repeat f n x =
    match n with
    | 1 -> f x
    | m -> f (repeat f (m-1) x);;
