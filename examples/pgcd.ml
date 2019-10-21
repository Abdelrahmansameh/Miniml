let abs = fun (x: int) ->
  if x <= 0 then -x
  else x in
let rec pgcdp = fun (p: int * int) ->
  let a = fst p in
  let b = snd p in
  if b = 0 then a
  else
    let q = a - b * (a / b) in
    pgcdp (b, q) in
let pgcd = fun (p: int * int) ->
  let a = fst p in
  let b = snd p in
  let a = abs a in
  let b = abs b in
  pgcdp (a, b) in
pgcd (96, -80)
