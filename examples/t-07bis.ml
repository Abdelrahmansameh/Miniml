let a = 1 in
let b = 1 in
let f = fun (b:int) -> a + b in
let a = 2 in
print_int(f 10 + a)