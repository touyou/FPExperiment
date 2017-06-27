(* Definition of "the" list monad *)
type 'a m = 'a list

(** (>>=) : 'a m -> ('a -> 'b m) -> 'b m *)
let (>>=) (x : 'a m) (f : 'a -> 'b m) =
  List.concat (List.map f x)

(** return : 'a -> 'a m *)
let return (x : 'a) = [x]

(** guard : bool -> unit m *)
let guard (x : bool) =
  if x then return () else []



(** Examples of boolean functions *)
(** SAT *)
let phi x y z =
  not(x)
  && (x || not(y))
  && (x || y || z)

(** UNSAT *)
let psi x y z =
  not(x)
  && (x || not(y))
  && (x || y || z)
  && (x || y || not(z))


