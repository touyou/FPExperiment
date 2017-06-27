type complex = { re : float; im : float; };;

let prod a b = { re = a.re *. b.re -. a.im *. b.im; im = a.re *. b.im +. b.re *. a.im };;

type str_tree =
  | Leaf
  | Node of string * str_tree * str_tree;;

let one_leaf = Leaf;;
let one_tree = Node ("hello", one_leaf, one_leaf);;
let tree = Node ("world", one_tree, Node ("!", one_leaf, one_leaf));;

type ib_list = INil
             | ICons of int * bi_list
and bi_list = BNil
             | BCons of bool * ib_list;;

let mynil = INil;;
let one_ib = ICons (1, BNil);;
let ibs = ICons (2, BCons (true, ICons (2, BNil)));;
