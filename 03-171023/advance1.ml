module type EQ = sig
  type ('a, 'b) equal
  val refl : ('a, 'a) equal
  val symm : ('a, 'b) equal -> ('b, 'a) equal
  val trans : ('a, 'b) equal -> ('b, 'c) equal -> ('a, 'c) equal
  val apply : ('a, 'b) equal -> 'a -> 'b
end

module Eq : EQ =
  struct
    type ('a, 'b) equal
  end
