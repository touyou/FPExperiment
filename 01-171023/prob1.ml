(* 0からnまでの和  *)
let rec sum_to n =
    match n with
    | 0 -> 0
    | m -> m + sum_to (m-1);;

(* nが素数かどうかを調べるため再帰でn以下の数字で割っていく。ループ用の関数を使う。 *)
let rec prime_loop n a =
    match a with
    | 2 -> n mod a = 0
    | m -> n mod m = 0 || prime_loop n (m-1);;

let is_prime n =
    match n with
    | 1 -> false
    | 2 -> true
    | m -> not (prime_loop m (m-1));;

(* ユークリッドの互除法  *)
let rec gcd a b =
    if a mod b = 0 then
        b
    else
        gcd b (a mod b);;
