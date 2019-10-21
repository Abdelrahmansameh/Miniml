{
 open Parser
let num_line = ref 1
let debug = false
let hash_string_tokens = Hashtbl.create 10
let _ =
  let lst_string_tokens =
    [ "bool",  T_bool  ;
      "else",  T_else  ;
      "false", T_false ;
      "fst",   T_fst   ;
      "fun",   T_fun   ;
      "if",    T_if    ;
      "in",    T_in    ;
      "int",   T_int   ;
      "let",   T_let   ;
      "not",   T_not   ;
      "rec",   T_rec   ;
      "snd",   T_snd   ;
      "then",  T_then  ;
      "true",  T_true
    ] in
  List.iter (fun (str, tok) -> Hashtbl.add hash_string_tokens str tok)
    lst_string_tokens
let retrieve_string_tok (s: string) =
  if debug then Printf.printf "string: %s\n" s;
  try Hashtbl.find hash_string_tokens s
  with Not_found -> V_string s
}

rule token = parse
| ' ' | '\t' | '#'['-']*  { token lexbuf }
| '\n'                    { incr num_line; token lexbuf }

| '/''/'[^'\n']*'\n'   (* line of comments *)
                          { incr num_line; token lexbuf }

| (['a'-'z']|['A'-'Z'])(['a'-'z']|['A'-'Z']|'_'|['0'-'9'])*
                          { let str = Lexing.lexeme lexbuf in
                            retrieve_string_tok str }
| (['0'-'9'])+            { let str = Lexing.lexeme lexbuf in
                            V_int (int_of_string str) }

| '='                     { if debug then Printf.printf "equal\n"; T_equal }
| '+'                     { T_add }
| '-'                     { T_sub }
| '*'                     { T_mul }
| '/'                     { T_div }
| "&&"                    { T_and }
| "<="                    { T_leq }

| '('                     { T_lpar }
| ')'                     { T_rpar }

| "->"                    { T_arrow }

| ':'                     { if debug then Printf.printf "colon\n"; T_colon }
| ','                     { T_comma }

| eof                     { T_eof }
