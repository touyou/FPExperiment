open Array
open Color
open Command


type board = color array array

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
    | _ -> board

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

let array_copy board =
  let c_board = Array.make_matrix 10 10 0 in
  for i = 0 to 9 do
    for j = 0 to 9 do
      let a = board.(i).(j) in
      c_board.(i).(j) <- a
    done
  done;
  c_board

let rec array_print board =
  for i = 1 to 8 do
    for j = 1 to 8 do
      print_int board.(i).(j);
      print_string " "
    done
  done;
  ()

(* let rec test_move board color ms =
  let i_board = array_copy board in
    match ms with
    | [] -> (1, 1)
    | (i, j) :: xs -> let i2_move = doMove i_board (Mv (i, j)) color in
                      test_move board color xs *)

let next_board board color i j =
  let i_board = array_copy board in
  doMove i_board (Mv (i, j)) color

let next_op_valid board color i j =
  let i_board = array_copy board in
  let n_board = doMove i_board (Mv (i,j)) color in
  valid_moves n_board (opposite_color color)

(* (0,0)が１つでもあるかどうか *)
let rec lookup_n yomikiri_list =
  match yomikiri_list with
  | [] -> (1, 1)
  | a :: xs -> if a = (0, 0) then (0, 0)
               else lookup_n xs

let rec lookup_n2 yomikiri_list =
  match yomikiri_list with
  | [] -> (0, 0)
  | a :: xs -> if a = (0, 0) then lookup_n2 xs
                             else a

let rec yomikiri_one_me board color a n =
  let (i, j) = n in
  let n_board = next_board board color i j in
  if (((count n_board none) = 0) && count n_board color >= a) then n
  else if count n_board none = 0 then (0, 0)
  else
  match n with
  | (i, j) -> if lookup_n (yomikiri_you n_board color a (next_op_valid board color i j)) = (0, 0) then (0, 0)
              else n

and yomikiri_you board color a op_ms =
  List.map (yomikiri_one_you board color a) op_ms

and yomikiri_one_you board color a n =
  let (i, j) = n in
  let n_board = next_board board (opposite_color color) i j in
  if (count n_board 0) = 0 && (count n_board color >= a) then n
  else if count n_board 0 = 0 then (0, 0)
  else
  match n with
  | (i, j) -> if lookup_n2 (yomikiri_me n_board color a (next_op_valid board (opposite_color color) i j)) = (0, 0) then (0, 0)
              else n

and yomikiri_me board color a ms =
  List.map (yomikiri_one_me board color a) ms



let next_op_valid_count board i j color =
  let i_board = array_copy board in
  let n_board = doMove i_board (Mv(i, j)) color in
  List.length (valid_moves n_board (opposite_color color))

let rec arround_count board i j =
  let sum = ref 0 in
  for a = i - 1 to i + 1 do
    for b = j - 1 to j - 1 do
      if board.(a).(b) = 1 || board.(a).(b) = 2 then sum := !sum + 1
    done
  done;
  !sum

let rec arround_sum board i j =
  let sum = ref 0 in
  for a = i - 1 to i + 1 do
    for b = j - 1 to j - 1 do
      sum := !sum + board.(a).(b)
    done;
  done;
  !sum

let rec gaishu_count board color =
  let sum = ref 0 in
  for a = 1 to 8 do
    if board.(1).(a) = color then sum := !sum + 1;
    if board.(8).(a) = color then sum := !sum + 1;
    if board.(a).(1) = color then sum := !sum + 1;
    if board.(a).(8) = color then sum := !sum + 1
  done;
  !sum


let eval_one board i j color =
  if (i = 1 || i = 8) && (j = 1 || j = 8) then 100
  else if (valid_moves (next_board board color i j) (opposite_color color)) = [] then 75
  else if ((i = 1 || i = 8 || j = 1 || j = 8) && arround_count board i j >= 5) || arround_count board i j >= 8 then 50
  else if ((i,j) = (1,2) || (i,j) = (1,7) || (i,j) = (2,1) || (i,j) = (2,2) || (i,j) = (2,7) || (i,j) = (2,8) || (i,j) = (7,1) || (i,j) = (7,2) || (i,j) = (7,7) || (i,j) = (7,8) || (i,j) = (8,2) || (i,j) = (8,7)) then -50
  (* else if (count board none > 15) && (i = 1 || i = 8 || j = 1 || j = 8) then -30 *)
  else if (i = 1 || i = 8 || j = 1 || j = 8) && (gaishu_count (next_board board color i j) (opposite_color color)) < gaishu_count board (opposite_color color)
    then (next_op_valid_count board i j (opposite_color color)) - (List.length (valid_moves board color)) + (List.length (valid_moves board (opposite_color color))) - (next_op_valid_count board i j color)
  else if (i = 1 || i = 8 || j = 1 || j = 8) && (count board none > 15) then (next_op_valid_count board i j (opposite_color color)) - (List.length (valid_moves board color)) + (List.length (valid_moves board (opposite_color color))) - (next_op_valid_count board i j color) - 4
  (* else if arround_count board i j >= 8 then 50 *)
  else (next_op_valid_count board i j (opposite_color color)) - (List.length (valid_moves board color)) + (List.length (valid_moves board (opposite_color color))) - (next_op_valid_count board i j color)

let rec eval_valid board color ms n max_n =
  match ms with
  | [] -> n
  | (i, j) :: xs -> let te_eval = eval_one board i j color in
                    if te_eval > max_n then eval_valid board color xs (i, j) te_eval
                    else if te_eval = max_n then let k = Random.int 2 in
                    (if k = 0 then eval_valid board color xs n max_n
                    else eval_valid board color xs (i, j) te_eval)
                    else eval_valid board color xs n max_n

let play board color =
  let ms = valid_moves board color in
    if ms = [] then
      Pass
    else
      (* let sub_board = array_copy board in
      let sub2_board = doMove sub_board (Mv (8, 8)) color in
      let (i, j) = test_move sub_board color ms in *)
      if (count board 0) < 11 then
        let (i, j) = lookup_n2 (yomikiri_me board color 33 ms) in
        (if (i, j) = (0, 0) then let (a,b) = eval_valid board color ms (0,0) (-100) in Mv (a,b)
        else Mv (i, j))
      else let (i,j) = eval_valid board color ms (0,0) (-100) in Mv (i,j)


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
