%{
open Ast
%}
%token T_eof

%token <bool>   V_bool
%token <int>    V_int
%token <string> V_string

%token T_bool T_int
%token T_else T_fun T_if T_in T_let T_rec T_then
%token T_false T_true
%token T_fst T_not T_snd

%token T_add T_sub T_mul T_div
%token T_equal
%token T_and
%token T_leq

%token T_lpar T_rpar
%token T_colon T_comma
%token T_arrow

%type <Ast.expr> prog

%start prog
%%

prog:
| expr T_eof { $1 }

expr:
| T_let V_string T_equal expr T_in expr
    { Elet (false, $2, $4, $6) }
| T_let T_rec V_string T_equal expr T_in expr
    { Elet (true, $3, $5, $7) }
| T_fun pars T_arrow expr
    { Efun ($2, $4) }
| T_if expr T_then expr T_else expr
    { Eif ($2, $4, $6) }
| expr0
    { $1 }

expr0:
| expr0 T_and expr1
    { Ebinary (Bband, $1, $3) }
| expr1
    { $1 }

expr1:
| expr1 T_leq expr2
    { Ebinary (Bcleq, $1, $3) }
| expr1 T_equal expr2
    { Ebinary (Bceq, $1, $3) }
| expr2
    { $1 }

expr2:
| expr2 T_add expr3
    { Ebinary (Biadd, $1, $3) }
| expr2 T_sub expr3
    { Ebinary (Bisub, $1, $3) }
| expr3
    { $1 }

expr3:
| expr3 T_mul expr4
    { Ebinary (Bimul, $1, $3) }
| expr3 T_div expr4
    { Ebinary (Bidiv, $1, $3) }
| expr4
    { $1 }

expr4:
| T_sub expr5
    { Eunary (Uineg, $2) }
| T_not expr5
    { Eunary (Ubnot, $2) }
| T_fst expr5
    { Eunary (Upfst, $2) }
| T_snd expr5
    { Eunary (Upsnd, $2) }
| expr5
    { $1 }

expr5:
| expr6
    { $1 }
| expr6 args
    { Eapply ($1, $2) }

expr6:
| const
    { Econst $1 }
| V_string
    { Ename $1 }
| T_lpar expr0 T_comma expr0 T_rpar
    { Epair ($2, $4) }
| T_lpar expr0 T_rpar
    { $2 }

const:
| bool
    { Cbool $1 }
| V_int
    { Cint $1 }

bool:
| T_false
    { false }
| T_true
    { true }

args:
| expr6
    { $1 :: [ ] }
| expr6 args
    { $1 :: $2 }

pars:
| par
    { $1 :: [ ] }
| par pars
    { $1 :: $2 }

par:
| T_lpar V_string T_colon typ T_rpar
    { $2, $4 }

typ:
| typ T_mul typ0
    { Tprod ($1, $3) }
| typ0
    { $1 }

typ0:
| T_int
    { Tint }
| T_bool
    { Tbool }
