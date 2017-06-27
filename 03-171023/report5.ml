module type SEMIRING =
  sig
    type t
    val add : t -> t -> t
    val mul : t -> t -> t
    val unit : t
    val zero : t
    val to_string : t -> string
  end

module type MATRIX =
  functor (T : SEMIRING) ->
    sig
      type t
      val empty : t
      val add : T.t list -> t -> t
      val mul : t -> t -> t
      val print_mat : t -> unit
    end

exception TypeError

module Matrix : MATRIX =
  functor (T : SEMIRING) -> struct
    type t = T.t list list

    let empty = []

    let add xs t = xs :: t

    (* reverse *)
    let rec rev arr acc =
      match arr with
      | [] -> acc
      | x :: xs -> rev xs (x :: acc)
    let reverse arr = rev arr []
    let rec mat_rev_sub mat acc =
      match mat with
      | [] -> acc
      | x :: xs -> mat_rev_sub xs ((reverse x) :: acc)
    let mat_rev mat = reverse (mat_rev_sub mat [])

    (* transpose 参考：http://www.ne.jp/asahi/music/marinkyo/funkcio/ocaml.html.ja *)
    let rec map f arr =
      match arr with
      | [] -> []
      | x :: xs -> f x :: map f xs
    let rec map2 f arrs =
      match arrs with
      | ([], _) -> []
      | (_, []) -> []
      | (x :: xs, y :: ys) -> f x y :: map2 f (xs, ys)
    let rec transpose mat =
      match mat with
      | [] -> []
      | [l] -> map (fun x -> [x]) l
      | v :: l -> map2 (fun x y -> x :: y) (v, (transpose l))

    let rec add_mul x v acc =
      match v with
      | [] -> acc
      | y :: ys -> add_mul x ys (T.add (T.mul x y) acc)
    let rec vec_mat vmat acc =
      match vmat with
      | ([], []) -> acc
      | ([], _) -> raise TypeError
      | (_, []) -> raise TypeError
      | (x :: xs, v :: l) -> vec_mat (xs, l) (add_mul x v T.zero :: acc)
    let rec mul_sub a b acc =
      match a with
      | [] -> acc
      | x :: xs -> mul_sub xs b ((vec_mat (x, b) []) :: acc)
    let mul a b = mat_rev (mul_sub a (transpose b) [])

    let rec print_sub y =
      match y with
      | [] -> ()
      | z :: zs -> print_string (T.to_string z); print_string " | "; print_sub zs
    let rec print_mat x =
      match x with
      | [] -> ()
      | y :: ys -> print_string "|"; print_sub y; print_string "\n"; print_mat ys

  end

module BooleanSemiring : SEMIRING =
  struct
    type t = bool
    let add x y = x || y
    let mul x y = x && y
    let unit = true
    let zero = false
    let to_string x = if x then "true" else "false"
  end

type infint =
  | Num of int
  | Inf

module IntSemiring : SEMIRING =
  struct
    type t = infint
    let add x y =
      match x with
      | Inf -> y
      | Num a -> match y with
        | Inf -> Num a
        | Num b -> if a > b then Num b else Num a
    let mul x y =
      match x with
      | Inf -> Inf
      | Num a -> match y with
        | Inf -> Inf
        | Num b -> Num (a + b)
    let unit = Num 1
    let zero = Inf
    let to_string x =
      match x with
      | Inf -> "inf"
      | Num a -> string_of_int a
  end

module BooleanMatrix = Matrix (BooleanSemiring)
module IntMatrix = Matrix (IntSemiring)

let x = BooleanMatrix.add [BooleanSemiring.unit; BooleanSemiring.zero] (BooleanMatrix.add [BooleanSemiring.unit; BooleanSemiring.zero] BooleanMatrix.empty);;
let y = BooleanMatrix.add [BooleanSemiring.unit; BooleanSemiring.zero] (BooleanMatrix.add [BooleanSemiring.zero; BooleanSemiring.zero] BooleanMatrix.empty);;
BooleanMatrix.print_mat x;;
BooleanMatrix.print_mat y;;
BooleanMatrix.print_mat (BooleanMatrix.mul x y);;
let z = IntMatrix.add [IntSemiring.unit; IntSemiring.unit] (IntMatrix.add [IntSemiring.unit; IntSemiring.unit] IntMatrix.empty);;
let a = IntMatrix.add [IntSemiring.unit; IntSemiring.zero] (IntMatrix.add [IntSemiring.zero; IntSemiring.unit] IntMatrix.empty);;
IntMatrix.print_mat z;;
IntMatrix.print_mat a;;
IntMatrix.print_mat (IntMatrix.mul z a);;
