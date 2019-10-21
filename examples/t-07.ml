let a = 1 in
let b = 1 in
let f = fun (b:int) -> a + b in
let a = 2 in
(f 10 + a)