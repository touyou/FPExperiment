type order = LT | EQ | GT

module type ORDERED_TYPE =
sig
  type t
  val compare : t -> t -> order
end


module type DICTIONARY =
  functor (T : ORDERED_TYPE) ->
    sig
      type t
      val empty     : t
      val add       : T.t -> 'a -> t -> t
      val remove    : T.t -> t -> t
      val lookup    : T.t -> t -> 'a
    end

type ('a, 'b) tree =
  | Leaf
  | Node of 'a * 'b * ('a, 'b) tree * ('a, 'b) tree
exception NotFound

module Dictionary : DICTIONARY =
  functor (T : ORDERED_TYPE) -> struct
    type t = (T.t, 'a) tree

    let empty = []

    let rec add x y tr =
      match tr with
      | Leaf -> Node (x, y, Leaf, Leaf)
      | Node (a, b, l, r) ->
          match T.compare x a with
          | LT -> Node (a, b, (add x y l), r)
          | EQ -> Node (x, y, l, r)
          | GT -> Node (a, b, l, (add x y r))

    let rec search_min tr =
      match tr with
      | Leaf -> raise NotFound
      | Node (x, y, Leaf, _) -> (x, y)
      | Node(x, y, l, r) -> search_min l
    let rec delete_min tr =
      match tr with
      | Leaf -> tr
      | Node (x, _, Leaf, r) -> r
      | Node (x, y, l, r) -> Node (x, y, (delete_min l), r)
    let rec remove x tr =
      match tr with
      | Leaf -> tr
      | Node (a, b, l, r) ->
          match T.compare x a with
          | LT -> Node (a, b, l, (remove x r))
          | EQ ->
              if l = Leaf then r
              else if r = Leaf then l
              else
                let (mkey, mdata) = search_min r in
                Node (mkey, mdata, l, (delete_min r))
          | GT -> Node (a, b, (remove x l), r)

    let rec lookup x tr =
      match tr with
      | Leaf -> raise NotFound
      | Node (a, b, l, r) ->
          match T.compare x a with
          | LT -> lookup x l
          | EQ -> b
          | GT -> lookup x r

  end

module OrderedString =
  struct
    type t = string
    let compare x y =
      let r = Pervasives.compare x y in
      if      r > 0 then GT
      else if r < 0 then LT
      else               EQ
  end

module StringDictionary =
  Dictionary (OrderedString)
