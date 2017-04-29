type false_t = { t : 'a. 'a };;
type 'a not_t = 'a -> false_t;;
type ('a, 'b) and_t = 'a * 'b;;
type ('a, 'b) or_t = L of 'a | R of 'b;;

(* callcc : (('a -> false_t) -> 'a) -> 'a *)
