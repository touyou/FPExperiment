type nat = Z | S of nat;;

let rec add a b =
  match a with
  | Z -> b
  | S x -> add x (S b);;

let rec sub a b =
  match b with
  | Z -> a
  | S x -> match a with
          | Z -> Z
          | S y -> sub y x;;

let rec mulacc a b s =
  match a with
  | Z -> b
  | S x -> mulacc x (add b s) s;;

let mul a b =
  match a with
  | Z -> b
  | S x -> mulacc x b b;;

let rec powacc a b s =
  match b with
  | Z -> a
  | S x -> powacc (mul a s) x s;;

let pow a b =
  match b with
  | Z -> S Z
  | S x -> powacc a x a;;

let rec n2iacc n m =
  match n with
  | Z -> m
  | S x -> n2iacc x (m + 1);;

let n2i n = n2iacc n 0;;

let rec i2nacc n m =
  match n with
  | 0 -> m
  | x -> i2nacc (x - 1) (S m);;

let i2n i = i2nacc i Z;;
