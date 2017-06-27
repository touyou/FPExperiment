type 'a m = (* Write here *)

(** (>>=) : 'a m -> ('a -> 'b m) -> 'b m *)
let (>>=) (x : 'a m) (f : 'a -> 'b m) = (* Write here *)

(** return : 'a -> 'a m *)
let return x = (* Write here *)

(** memo : (int -> int m) -> int -> int m *)
let memo (f : int -> int m) n = (* Write here *)

(** runMemo : 'a m -> 'a *)
let runMemo (x : 'a m) = (* Write here *)
	
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
