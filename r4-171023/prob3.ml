type 'a m = 'a * string

(** (>>=) : 'a m -> ('a -> 'b m) -> 'b m *)
let (>>=) (x : 'a m) (f : 'a -> 'b m) =
  let (v, str) = x in
  let (newv, nstr) = f v in
  (newv, str ^ nstr)

(** return : 'a -> 'a m *)
let return (x : 'a) = (x, "")

(** writer : string -> unit m *)
let writer (m : string) = ((), m)

let msg n = ("Fib(" ^ (string_of_int n) ^")\n")

(** fib : int -> int m *)
let rec fib n =
  (writer (msg n)) >>= (fun _ ->
  if n <= 1 then
    return n
  else
    (fib (n-2)) >>= (fun x ->
    (fib (n-1)) >>= (fun y ->
    return (x + y))))

let _ =
  let (_, m) = fib 4 in
  print_string m

