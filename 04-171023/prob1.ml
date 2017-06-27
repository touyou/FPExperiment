type 'a myErr = Ok of 'a | Err of string
let (>>=) x f =
  match x with
  | Err msg -> Err msg
  | Ok y -> f y

let rec eLookup key xs =
  match xs with
  | [] -> Err "Not Found"
  | ((k,v)::rest) ->
      if k = key then Ok v
      else eLookup key rest;;

let myDiv x y =
  if y = 0 then Err "Div by Zero"
  else Ok (x / y);;

let lookupDiv kx ky t =
  (eLookup kx t) >>= (fun x ->
  (eLookup ky t) >>= (fun y ->
    myDiv x y));;
