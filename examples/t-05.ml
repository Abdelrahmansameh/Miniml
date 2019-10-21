let f = fun (x: int) -> x + 1 in
let g = fun (x: int) -> f x in
let f = fun (x: int) -> 8 in
g (f 0)
