module Multiset =
struct 
  type 'a t = 'a list 
  let empty = []
  let add a xs = a::xs
  let rec remove a xs = 
    match xs with 
	[]    -> []
      | y::ys -> if a=y then ys else y::remove a ys
  let rec count_sub a xs k =
    match xs with 
	[]     -> k 
      | y::ys  -> 
	if a=y then count_sub a ys (k+1) 
	else        count_sub a ys k 
  let count a xs = count_sub a xs 0 
end

module type MULTISET =
sig
  type 'a t 
  val empty : 'a t 
  val add : 'a -> 'a t -> 'a t 
  val remove : 'a -> 'a t -> 'a t 
  val count : 'a -> 'a t -> int 
end

