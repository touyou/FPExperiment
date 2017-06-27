type order = LT | EQ | GT

module type ORDERED_TYPE =
sig
  type t
  val compare : t -> t -> order
end

module type MULTISET2 =
  functor (T : ORDERED_TYPE) ->
    sig
      type t
      val empty  : t
      val add    : T.t -> t -> t
      val remove : T.t -> t -> t
      val count  : T.t -> t -> int
    end

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

exception NotFound

(* 二分木での実装 *)
module Multiset2 : MULTISET2 =
  functor (T : ORDERED_TYPE) -> struct
    type t = T.t tree
    let empty = Leaf

    let rec add x tr =
      match tr with
      | Leaf -> Node (x, Leaf, Leaf)
      | Node (y, l, r) ->
          match T.compare x y with
          | LT -> Node (y, (add x l), r)
          | EQ -> Node (y, (add x l), r)
          | GT -> Node (y, l, (add x r))

    let rec search_min tr =
      match tr with
      | Leaf -> raise NotFound
      | Node (x, Leaf, _) -> x
      | Node (_, l, _) -> search_min l
    let rec delete_min tr =
      match tr with
      | Leaf -> tr
      | Node (x, Leaf, r) -> r
      | Node (x, l, r) -> Node (x, (delete_min l), r)
    let rec remove x tr =
      match tr with
      | Leaf -> tr
      | Node (y, l, r) ->
          match T.compare x y with
          | GT -> Node (y, l, (remove x r))
          | EQ ->
              if l = Leaf then r
              else if r = Leaf then l
              else
                let min_d = search_min r in
                Node (min_d, l, (delete_min r))
          | LT -> Node (y, (remove x l), r)

    let rec count_sub x tr n =
      match tr with
      | Leaf -> n
      | Node (y, l, r) ->
          match T.compare x y with
          | LT -> count_sub x l n
          | EQ -> count_sub x l (n+1)
          | GT -> count_sub x r n
    let count x tr = count_sub x tr 0
  end

module OrderedInt = struct
  type t = int
  let compare a b =
    if a < b then LT
    else if a > b then GT
    else EQ
end

module IntMultiset =
  Multiset2 (OrderedInt)
