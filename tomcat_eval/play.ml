open Array 
open Color 
open Command 


type board = color array array 

let inf = 1e64

let copy_board board =
  Array.init (Array.length board) (fun i -> Array.copy board.(i))

let eq_color c1 c2 = if c1 = c2 then true else false

let init_board () = 
  let board = Array.make_matrix 10 10 none in 
    for i=0 to 9 do 
      board.(i).(0) <- sentinel ;
      board.(i).(9) <- sentinel ;
      board.(0).(i) <- sentinel ;
      board.(9).(i) <- sentinel ;
    done;
    board.(4).(4) <- white;
    board.(5).(5) <- white;
    board.(4).(5) <- black;
    board.(5).(4) <- black;
    board 

let init_table () =
  let table = Array.make_matrix 10 10 none in 
    for i=0 to 9 do 
      table.(i).(0) <- sentinel ;
      table.(i).(9) <- sentinel ;
      table.(0).(i) <- sentinel ;
      table.(9).(i) <- sentinel ;
    done;
    table

let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ]

let flippable_indices_line board color (di,dj) (i,j) =
  let ocolor = opposite_color color in
  let rec f (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then 
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else 
      [] 
  and    g (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then 
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else if board.(i).(j) = color then 
      r
    else 
      [] in 
    f (di,dj) (i,j) []

let flippable_indices board color (i,j) =
  let bs = List.map (fun (di,dj) -> flippable_indices_line board color (di,dj) (i+di,j+dj)) dirs in 
    List.concat bs 
    
let is_effective board color (i,j) =
  match flippable_indices board color (i,j) with 
      [] -> false
    | _  -> true 

let is_valid_move board color (i,j) =
  (board.(i).(j) = none) && is_effective board color (i,j) 

let doMove board com color =
  match com with 
      GiveUp  -> board
    | Pass    -> board
    | Mv (i,j) -> 
	let ms = flippable_indices board color (i,j) in 
	let _  = List.map (fun (ii,jj) -> board.(ii).(jj) <- color) ms in 
	let _  = board.(i).(j) <- color in 
	  board 

let mix xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)
	     
let valid_moves board color = 
  let ls = [1;2;3;4;5;6;7;8] in 
  List.filter (is_valid_move board color)
    (mix ls ls)

let count board color = 
  let s = ref 0 in 
    for i=1 to 8 do 
      for j=1 to 8 do
        if board.(i).(j) = color then s := !s + 1 
      done
    done;
    !s

let rnd () = Random.float 1.0

let board_weight = [| [|  0;   0;   0;   0;   0;   0;   0;   0;   0;   0|];
                      [|  0;  45; -11;   4;  -1;  -1;   4; -11;  45;   0|];
                      [|  0; -11; -16;  -1;  -3;  -3;  -1; -16; -11;   0|];
                      [|  0;   4;  -1;   2;  -1;  -1;   2;  -1;   4;   0|];
                      [|  0;  -1;  -3;  -1;   0;   0;  -1;  -3;  -1;   0|];
                      [|  0;  -1;  -3;  -1;   0;   0;  -1;  -3;  -1;   0|];
                      [|  0;   4;  -1;   2;  -1;  -1;   2;  -1;   4;   0|];
                      [|  0; -11; -16;  -1;  -3;  -3;  -1; -16; -11;   0|];
                      [|  0;  45; -11;   4;  -1;  -1;   4; -11;  45;   0|];
                      [|  0;   0;   0;   0;   0;   0;   0;   0;   0;   0|] |]

let eval_board board color =
  let ocolor = opposite_color color in
  let score = ref 0.0 in 
    for i=1 to 8 do 
      for j=1 to 8 do
        let w = if board.(i).(j) = color then 1.0 else if board.(i).(j) = ocolor then -1.0 else 0.0 in
        score := !score +. float_of_int board_weight.(i).(j) *. w
      done
    done;
    !score *. (2.5 +. rnd ())

let fixed_stones board =
  let upper_left = init_table () in
    for i = 1 to 8 do
      for j = 1 to 8 do
        let c = board.(i).(j) in
        if      eq_color c upper_left.(i-1).(j) && eq_color c upper_left.(i).(j-1) then
          upper_left.(i).(j) <- c
      done;
    done;

  let upper_right = init_table () in
    for i = 1 to 8 do
      for j = 8 to 1 do
        let c = board.(i).(j) in
        if      eq_color c upper_right.(i-1).(j) && eq_color c upper_right.(i).(j+1) then
          upper_right.(i).(j) <- c
      done;
    done;
    
  let lower_left = init_table () in
    for i = 8 to 1 do
      for j = 1 to 8 do
        let c = board.(i).(j) in
        if      eq_color c lower_left.(i+1).(j) && eq_color c lower_left.(i).(j-1) then
          lower_left.(i).(j) <- c
      done;
    done;

  let lower_right = init_table () in
    for i = 8 to 1 do
      for j = 8 to 1 do
        let c = board.(i).(j) in
        if      eq_color c lower_right.(i+1).(j) && eq_color c lower_right.(i).(j+1) then
          lower_right.(i).(j) <- c
      done;
    done;

  let table = init_table () in
  for i = 1 to 8 do
    for j = 1 to 8 do
      if      eq_color upper_left.(i).(j) white || eq_color upper_right.(i).(j) white
           || eq_color lower_left.(i).(j) white || eq_color lower_right.(i).(j) white then
        table.(i).(j) <- white
      else if eq_color upper_left.(i).(j) black || eq_color upper_right.(i).(j) black
           || eq_color lower_left.(i).(j) black || eq_color lower_right.(i).(j) black then
        table.(i).(j) <- black
    done;
  done;
  table

let eval_fixed_stone board color =
  let ocolor = opposite_color color in
  let table = fixed_stones board in
  float_of_int (count table color - count table ocolor) *. (12.5 +. rnd())

let eval board color =
  let b = eval_board board color in
  let f = eval_fixed_stone board color in
  b +. 3.0 *. f

let eval_move board color (i, j) =
  let nboard = copy_board board in
  let nboard = doMove nboard (Mv (i, j)) color in
  eval nboard color

let rec argmax f = function
  | [] -> ((0, 0), 0.0)
  | [x] -> (x, f x)
  | x :: dom ->
      let fx = f x in
      let (y, fy) = argmax f dom in
      if fx > fy then (x, fx) else (y, fy)

let play board color = 
  let ms = valid_moves board color in 
    if ms = [] then 
      Pass 
    else 
      let ((i, j), _) = argmax (eval_move board color) ms in
      Mv (i, j)

      (*
let play board color =
  let nega_alpha board color depth alpha beta =
    if depth = 0 then eval board color
    else
      let ms = valid_moves board color in
      if ms = [] then eval board color
      else
        List.fold_left
    let ms = valid_moves board color in
    if depth = 0 || 
    *)

let print_board board = 
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j=1 to 8 do 
    print_int j; print_string "|";
    for i=1 to 8 do 
      print_color (board.(i).(j)); print_string " " 
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)"
      
let report_result board = 
  let _ = print_endline "========== Final Result ==========" in 
  let bc = count board black in 
  let wc = count board white in 
    if bc > wc then 
      print_endline "*Black wins!*" 
    else if bc < wc then 
      print_endline "*White wins!*" 
    else
      print_endline "*Even*";
    print_string "Black: "; print_endline (string_of_int bc);
    print_string "White: "; print_endline (string_of_int wc);
    print_board board 

