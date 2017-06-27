let rec fix f x = f (fix f) x;;

(* nまでの和 sum_to *)
let sum_to n = fix (fun f x -> if x = 0 then 0 else x + f (x-1)) n;;

(* nが素数かどうか is_prime *)
let is_prime n =
    match n with
    | 1 -> false
    | 2 -> true
    | m -> not (fix (fun f (x, y) -> if y = 2 then x mod y = 0 else x mod y = 0 || f (x, y-1)) (m, m-1));;

(* gcd *)
let gcd a b = fix (fun f (x, y) -> if x mod y = 0 then y else f (y, x mod y)) (a, b);;
