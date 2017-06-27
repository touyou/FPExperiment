module type EQ =
  sig
    type ('a, 'b) equal
    val refl : ('a, 'a) equal
    val symm : ('a, 'b) equal -> ('b, 'a) equal
    val trans : ('a, 'b) equal -> ('b, 'c) equal -> ('a, 'c) equal
    val apply : ('a, 'b) equal -> 'a -> 'b
  end

module Eq : EQ

type 'a value =
  | VBool of (bool, 'a) Eq.equal * bool
  | VInt of (int, 'a) Eq.equal * int

type 'a expr =
  | EConstInt of (int, 'a) Eq.equal * int
  | EAdd of (int, 'a) Eq.equal * (int expr) * (int expr)
  | ESub of (int, 'a) Eq.equal * (int expr) * (int expr)
  | EMul of (int, 'a) Eq.equal * (int expr) * (int expr)
  | EDiv of (int, 'a) Eq.equal * (int expr) * (int expr)
  | EConstBool of (bool, 'a) Eq.equal * bool
  | EEq of (bool, 'a) Eq.equal * (int expr) * (int expr)
  | ELt of (bool, 'a) Eq.equal * (int expr) * (int expr)
  | EIf of bool expr * 'a expr * 'a expr

val eval : 'a expr -> 'a value

