type 'a m = 'a list;;
let (>>=) x f = List.concat (List.map f x);;
let return x = [x];;
let guard b = if b then return () else [];;

let numa = [0;1;2;3;4;5;6;7;8;9];;
let nums = [1;2;3;4;5;6;7;8;9];;
let nosame a b c d e = a != b && a != c && a != d && a != e && b != c && b != d && b != e && c != d && c != e && d!= e;;
let bans = numa >>= (fun a ->
           numa >>= (fun b ->
           numa >>= (fun c ->
           numa >>= (fun d ->
           numa >>= (fun e ->
  (guard (((a*100+b*11)*2 = c*1000+b*100+d*10+e) && a != 0 && c != 0 && (nosame a b c d e))) >>= (fun _ ->
    return (a, b, c, d, e)))))));;

(* send+more=money *)
let rec filter f arr =
  match arr with
  | [] -> []
  | x :: xs -> if f x then x :: filter f xs else filter f xs;;

let sendmoremoney =
  nums >>= (fun s ->
  (filter (fun x -> x != s) nums) >>= (fun m ->
  (filter (fun x -> x != s && x != m) numa) >>= (fun e ->
  (filter (fun x -> x != s && x != m && x != e) numa) >>= (fun n ->
  (filter (fun x -> x != s && x != m && x != e && x != n) numa) >>= (fun d ->
  (filter (fun x -> x != s && x != m && x != e && x != n && x != d) numa) >>= (fun o ->
  (filter (fun x -> x != s && x != m && x != e && x != n && x != d && x != o) numa) >>= (fun r ->
  (filter (fun x -> x != s && x != m && x != e && x != n && x != d && x != o && x != r) numa) >>= (fun y ->
  (guard (s*1000+e*101+n*10+d+m*1000+o*100+r*10=m*10000+o*1000+n*100+e*10+y)) >>= (fun _ ->
    return (s,e,n,d,m,o,r,e,m,o,n,e,y))))))))));;
