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
      val empty : t
      val add    : T.t -> t -> t
      val remove : T.t -> t -> t
      val count  : T.t -> t -> int
    end

module Multiset2 : MULTISET2 =
  functor (T : ORDERED_TYPE) -> struct
    type t = T.t list
    let rec remove a xs =
      match xs with
	| [] -> []
	| y :: ys ->
	  (match T.compare a y with
	    | LT -> y :: ys
	    | EQ -> ys
	    | GT -> y :: remove a ys)
    let empty = []
    let rec add a xs =
      match xs with
      | [] -> [a]
      | y :: ys ->
	 (match T.compare a y with
	  | LT -> a :: y :: ys
	  | EQ -> a :: y :: ys
	  | GT -> y :: add a ys)
    let rec count_sub a xs k =
      match xs with
	  []     -> k
	| y::ys  ->
	   (match T.compare a y with
	    | LT -> k
	    | EQ -> count_sub a ys (k+1)
	    | GT -> count_sub a ys k)
    let count a xs = count_sub a xs 0
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
