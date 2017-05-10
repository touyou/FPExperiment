module type LISTSTRUCT =
sig
  type 'a t
  val pop : 'a t -> ('a * 'a t)
  val push : 'a -> 'a t -> 'a t
  val empty : 'a t
  val size : 'a t -> int
end

exception EmptyStack

module Stack : LISTSTRUCT =
struct
  type 'a t = 'a list
  let empty = []

  let pop xs =
    match xs with
    | [] -> raise EmptyStack
    | y :: ys -> (y, ys)

  let push a xs = a :: xs

  let rec size_sub xs n =
    match xs with
    | [] -> n
    | y :: ys -> size_sub ys (n+1)
  let size xs = size_sub xs 0
end

