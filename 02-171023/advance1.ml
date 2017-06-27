let fix f a =
  let g = ref f in
  g := !g f; !g a;;
