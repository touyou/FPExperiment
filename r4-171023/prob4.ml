type 'a maybe = Just of 'a | Nothing

let rec get x s =
  match s with
  | [] -> Nothing
  | (y, v) :: ys -> if x = y then v else get x ys

type 'a memo = ('a * 'a maybe) list

type 'a m = 'a memo -> ('a * 'a memo)

(** (>>=) : 'a m -> ('a -> 'b m) -> 'b m *)
let (>>=) (x : 'a m) (f : 'a -> 'b m) =
  fun s -> let (a, s1) = x s in (f a) s1

(** return : 'a -> 'a m *)
let return x = fun s -> (x, s)

(** memo : (int -> int m) -> int -> int m *)
let memo (f : int -> int m) n =
  fun s ->
    match get n s with
    | Just v -> (v, s)
    | Nothing -> let (v, s1) = (f n) s in (v, (n, Just v) :: s1)

(** runMemo : 'a m -> 'a *)
let runMemo (x : 'a m) = let (res, mem) = x [] in res

let rec fib n =
  if n <= 1 then
    return n
  else
    (memo fib (n-2)) >>= (fun r1 ->
    (memo fib (n-1)) >>= (fun r2 ->
      return (r1 + r2)))

let _ =
  if runMemo (fib 80) = 23416728348467685 && runMemo (fib 10) = 55 then
    print_string "ok\n"
  else
    print_string "wrong\n"
