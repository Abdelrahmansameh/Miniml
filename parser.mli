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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
