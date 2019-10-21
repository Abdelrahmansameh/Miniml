let f = fun (x: int) -> x + 1 in
let g = fun (x: int) -> f x in
let f = fun (x: int) (y: bool) -> if y then 8 else 12 in
g (f 0 false)
