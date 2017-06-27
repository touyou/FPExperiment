(* addは簡単 *)
let add n m f x = n f (m f x);;

(* mulは引数の渡し方を変える *)
let mul n m f x = n (fun x -> m f x) x;;

(* subは *)
let sub n m f x =
