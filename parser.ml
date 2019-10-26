type token =
  | T_eof
  | V_bool of (bool)
  | V_int of (int)
  | V_string of (string)
  | T_bool
  | T_int
  | T_else
  | T_fun
  | T_if
  | T_in
  | T_let
  | T_rec
  | T_then
  | T_false
  | T_true
  | T_fst
  | T_not
  | T_snd
  | T_add
  | T_sub
  | T_mul
  | T_div
  | T_equal
  | T_and
  | T_leq
  | T_lpar
  | T_rpar
  | T_colon
  | T_comma
  | T_arrow

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 38 "parser.ml"
let yytransl_const = [|
  257 (* T_eof *);
  261 (* T_bool *);
  262 (* T_int *);
  263 (* T_else *);
  264 (* T_fun *);
  265 (* T_if *);
  266 (* T_in *);
  267 (* T_let *);
  268 (* T_rec *);
  269 (* T_then *);
  270 (* T_false *);
  271 (* T_true *);
  272 (* T_fst *);
  273 (* T_not *);
  274 (* T_snd *);
  275 (* T_add *);
  276 (* T_sub *);
  277 (* T_mul *);
  278 (* T_div *);
  279 (* T_equal *);
  280 (* T_and *);
  281 (* T_leq *);
  282 (* T_lpar *);
  283 (* T_rpar *);
  284 (* T_colon *);
  285 (* T_comma *);
  286 (* T_arrow *);
    0|]

let yytransl_block = [|
  258 (* V_bool *);
  259 (* V_int *);
  260 (* V_string *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\004\000\004\000\
\005\000\005\000\005\000\006\000\006\000\006\000\007\000\007\000\
\007\000\008\000\008\000\008\000\008\000\008\000\009\000\009\000\
\010\000\010\000\010\000\010\000\012\000\012\000\013\000\013\000\
\011\000\011\000\003\000\003\000\014\000\015\000\015\000\016\000\
\016\000\000\000"

let yylen = "\002\000\
\002\000\006\000\007\000\004\000\006\000\001\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\001\000\003\000\003\000\
\001\000\002\000\002\000\002\000\002\000\001\000\001\000\002\000\
\001\000\001\000\005\000\003\000\001\000\001\000\001\000\001\000\
\001\000\002\000\001\000\002\000\005\000\003\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\030\000\026\000\000\000\000\000\000\000\031\000\
\032\000\000\000\000\000\000\000\000\000\000\000\042\000\000\000\
\000\000\000\000\000\000\000\000\017\000\022\000\000\000\025\000\
\029\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
\019\000\021\000\018\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\000\000\000\000\000\
\036\000\000\000\000\000\000\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\000\016\000\034\000\000\000\004\000\
\000\000\000\000\000\000\000\000\041\000\040\000\000\000\039\000\
\000\000\000\000\000\000\027\000\000\000\037\000\005\000\002\000\
\000\000\038\000\003\000"

let yydgoto = "\002\000\
\015\000\016\000\027\000\017\000\018\000\019\000\020\000\021\000\
\022\000\023\000\046\000\024\000\025\000\028\000\071\000\072\000"

let yysindex = "\006\000\
\165\255\000\000\000\000\000\000\007\255\165\255\044\255\000\000\
\000\000\032\255\032\255\032\255\032\255\183\255\000\000\076\255\
\056\255\250\254\019\255\070\255\000\000\000\000\032\255\000\000\
\000\000\081\255\064\255\007\255\093\255\090\255\114\255\000\000\
\000\000\000\000\000\000\047\255\000\000\183\255\183\255\183\255\
\183\255\183\255\183\255\183\255\032\255\000\000\095\255\165\255\
\000\000\165\255\165\255\104\255\000\000\183\255\250\254\019\255\
\019\255\070\255\070\255\000\000\000\000\000\000\091\255\000\000\
\122\255\125\255\165\255\046\255\000\000\000\000\238\254\000\000\
\165\255\165\255\134\255\000\000\091\255\000\000\000\000\000\000\
\165\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\003\255\005\255\109\255\059\255\000\000\000\000\001\255\000\000\
\000\000\000\000\000\000\126\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\030\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\088\255\130\255\
\138\255\080\255\101\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\124\000\243\255\120\000\102\000\105\000\106\000\
\052\000\238\255\115\000\000\000\000\000\000\000\000\000\087\000"

let yytablesize = 209
let yytable = "\029\000\
\036\000\023\000\077\000\006\000\045\000\008\000\001\000\023\000\
\078\000\006\000\023\000\008\000\006\000\023\000\008\000\006\000\
\039\000\008\000\040\000\023\000\023\000\023\000\023\000\023\000\
\023\000\023\000\045\000\023\000\008\000\023\000\033\000\008\000\
\026\000\008\000\003\000\004\000\033\000\041\000\042\000\033\000\
\068\000\064\000\033\000\065\000\066\000\008\000\009\000\030\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\031\000\
\033\000\014\000\033\000\014\000\075\000\032\000\033\000\034\000\
\035\000\014\000\079\000\080\000\014\000\038\000\038\000\014\000\
\076\000\053\000\083\000\054\000\037\000\014\000\014\000\038\000\
\012\000\014\000\014\000\014\000\047\000\014\000\012\000\014\000\
\007\000\012\000\043\000\044\000\012\000\048\000\007\000\069\000\
\070\000\007\000\012\000\012\000\007\000\013\000\012\000\012\000\
\012\000\050\000\012\000\013\000\012\000\011\000\013\000\007\000\
\051\000\013\000\007\000\011\000\007\000\052\000\011\000\013\000\
\013\000\011\000\063\000\013\000\013\000\013\000\067\000\013\000\
\073\000\013\000\010\000\011\000\011\000\011\000\074\000\011\000\
\010\000\011\000\009\000\010\000\056\000\057\000\010\000\081\000\
\009\000\058\000\059\000\009\000\060\000\061\000\009\000\049\000\
\010\000\010\000\010\000\035\000\010\000\055\000\010\000\062\000\
\009\000\009\000\009\000\082\000\009\000\000\000\009\000\003\000\
\004\000\000\000\000\000\000\000\005\000\006\000\000\000\007\000\
\000\000\000\000\008\000\009\000\010\000\011\000\012\000\000\000\
\013\000\003\000\004\000\000\000\000\000\000\000\014\000\000\000\
\000\000\000\000\000\000\000\000\008\000\009\000\010\000\011\000\
\012\000\000\000\013\000\000\000\000\000\000\000\000\000\000\000\
\014\000"

let yycheck = "\006\000\
\014\000\001\001\021\001\001\001\023\000\001\001\001\000\007\001\
\027\001\007\001\010\001\007\001\010\001\013\001\010\001\013\001\
\023\001\013\001\025\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\045\000\027\001\024\001\029\001\001\001\027\001\
\026\001\029\001\003\001\004\001\007\001\019\001\020\001\010\001\
\054\000\048\000\013\001\050\000\051\000\014\001\015\001\004\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\012\001\
\027\001\026\001\029\001\001\001\067\000\010\000\011\000\012\000\
\013\000\007\001\073\000\074\000\010\001\024\001\024\001\013\001\
\027\001\027\001\081\000\029\001\001\001\019\001\020\001\024\001\
\001\001\023\001\024\001\025\001\004\001\027\001\007\001\029\001\
\001\001\010\001\021\001\022\001\013\001\030\001\007\001\005\001\
\006\001\010\001\019\001\020\001\013\001\001\001\023\001\024\001\
\025\001\013\001\027\001\007\001\029\001\001\001\010\001\024\001\
\023\001\013\001\027\001\007\001\029\001\004\001\010\001\019\001\
\020\001\013\001\028\001\023\001\024\001\025\001\023\001\027\001\
\007\001\029\001\001\001\023\001\024\001\025\001\010\001\027\001\
\007\001\029\001\001\001\010\001\039\000\040\000\013\001\010\001\
\007\001\041\000\042\000\010\001\043\000\044\000\013\001\028\000\
\023\001\024\001\025\001\030\001\027\001\038\000\029\001\045\000\
\023\001\024\001\025\001\077\000\027\001\255\255\029\001\003\001\
\004\001\255\255\255\255\255\255\008\001\009\001\255\255\011\001\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\255\255\
\020\001\003\001\004\001\255\255\255\255\255\255\026\001\255\255\
\255\255\255\255\255\255\255\255\014\001\015\001\016\001\017\001\
\018\001\255\255\020\001\255\255\255\255\255\255\255\255\255\255\
\026\001"

let yynames_const = "\
  T_eof\000\
  T_bool\000\
  T_int\000\
  T_else\000\
  T_fun\000\
  T_if\000\
  T_in\000\
  T_let\000\
  T_rec\000\
  T_then\000\
  T_false\000\
  T_true\000\
  T_fst\000\
  T_not\000\
  T_snd\000\
  T_add\000\
  T_sub\000\
  T_mul\000\
  T_div\000\
  T_equal\000\
  T_and\000\
  T_leq\000\
  T_lpar\000\
  T_rpar\000\
  T_colon\000\
  T_comma\000\
  T_arrow\000\
  "

let yynames_block = "\
  V_bool\000\
  V_int\000\
  V_string\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 30 "parser.mly"
             ( _1 )
# 240 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
    ( Elet (false, _2, _4, _6) )
# 249 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
    ( Elet (true, _3, _5, _7) )
# 258 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'pars) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
    ( Efun (_2, _4) )
# 266 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
    ( Eif (_2, _4, _6) )
# 275 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr0) in
    Obj.repr(
# 42 "parser.mly"
    ( _1 )
# 282 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr0) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr1) in
    Obj.repr(
# 46 "parser.mly"
    ( Ebinary (Bband, _1, _3) )
# 290 "parser.ml"
               : 'expr0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr1) in
    Obj.repr(
# 48 "parser.mly"
    ( _1 )
# 297 "parser.ml"
               : 'expr0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr1) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 52 "parser.mly"
    ( Ebinary (Bcleq, _1, _3) )
# 305 "parser.ml"
               : 'expr1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr1) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 54 "parser.mly"
    ( Ebinary (Bceq, _1, _3) )
# 313 "parser.ml"
               : 'expr1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 56 "parser.mly"
    ( _1 )
# 320 "parser.ml"
               : 'expr1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr2) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 60 "parser.mly"
    ( Ebinary (Biadd, _1, _3) )
# 328 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr2) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 62 "parser.mly"
    ( Ebinary (Bisub, _1, _3) )
# 336 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 64 "parser.mly"
    ( _1 )
# 343 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 68 "parser.mly"
    ( Ebinary (Bimul, _1, _3) )
# 351 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 70 "parser.mly"
    ( Ebinary (Bidiv, _1, _3) )
# 359 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 72 "parser.mly"
    ( _1 )
# 366 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 76 "parser.mly"
    ( Eunary (Uineg, _2) )
# 373 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 78 "parser.mly"
    ( Eunary (Ubnot, _2) )
# 380 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 80 "parser.mly"
    ( Eunary (Upfst, _2) )
# 387 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 82 "parser.mly"
    ( Eunary (Upsnd, _2) )
# 394 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 84 "parser.mly"
    ( _1 )
# 401 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 88 "parser.mly"
    ( _1 )
# 408 "parser.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr6) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 90 "parser.mly"
    ( Eapply (_1, _2) )
# 416 "parser.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 94 "parser.mly"
    ( Econst _1 )
# 423 "parser.ml"
               : 'expr6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
    ( Ename _1 )
# 430 "parser.ml"
               : 'expr6))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr0) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 98 "parser.mly"
    ( Epair (_2, _4) )
# 438 "parser.ml"
               : 'expr6))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 100 "parser.mly"
    ( _2 )
# 445 "parser.ml"
               : 'expr6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool) in
    Obj.repr(
# 104 "parser.mly"
    ( Cbool _1 )
# 452 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 106 "parser.mly"
    ( Cint _1 )
# 459 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
    ( false )
# 465 "parser.ml"
               : 'bool))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
    ( true )
# 471 "parser.ml"
               : 'bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 116 "parser.mly"
    ( _1 :: [ ] )
# 478 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr6) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 118 "parser.mly"
    ( _1 :: _2 )
# 486 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'par) in
    Obj.repr(
# 122 "parser.mly"
    ( _1 :: [ ] )
# 493 "parser.ml"
               : 'pars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'par) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pars) in
    Obj.repr(
# 124 "parser.mly"
    ( _1 :: _2 )
# 501 "parser.ml"
               : 'pars))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 128 "parser.mly"
    ( _2, _4 )
# 509 "parser.ml"
               : 'par))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ0) in
    Obj.repr(
# 132 "parser.mly"
    ( Tprod (_1, _3) )
# 517 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ0) in
    Obj.repr(
# 134 "parser.mly"
    ( _1 )
# 524 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "parser.mly"
    ( Tint )
# 530 "parser.ml"
               : 'typ0))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
    ( Tbool )
# 536 "parser.ml"
               : 'typ0))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
