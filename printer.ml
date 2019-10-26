open Ast

let p_name oc (name: string) =                                  (*print a string*)
    Printf.fprintf oc "%s" name;;

let p_const oc const = match const with                         (*const is either int or bool*)
                    |Cint(x) -> Printf.fprintf oc "%d" x;
                    |Cbool(b) -> Printf.fprintf oc "%b" b;
;;
let p_uop oc uop = match uop with                               (*Unary operations*)
                  |Uineg -> Printf.fprintf oc "-";
                  |Ubnot -> Printf.fprintf oc "!";
                  |Upfst -> Printf.fprintf oc "first";
                  |Upsnd -> Printf.fprintf oc "second";
;;
let p_bop oc bop = match bop with                               (*Binary operations*)
                  |Biadd -> Printf.fprintf oc "+";
                  |Bisub -> Printf.fprintf oc "-";
                  |Bimul -> Printf.fprintf oc "*";
                  |Bidiv -> Printf.fprintf oc "/";
                  |Bband -> Printf.fprintf oc "&";
                  |Bcleq -> Printf.fprintf oc "<=";
                  |Bceq -> Printf.fprintf oc "==";
;;

let rec p_typ oc (typ: typ) =                                   (*Types*)
    let rec p_ltyp oc ltyp = match ltyp with                    (*print recursively a list of types*)
        | [] -> Printf.fprintf oc "";                               (*base case*)
        | h::b -> begin
                    p_typ oc h;                                     (*first element*)
                    Printf.fprintf oc " ";
                    p_ltyp oc b;                                    (*rest of the list*)
                end;
    in
    match typ with                                              (*case for different types*)
        |Tint -> Printf.fprintf oc "int";
        |Tbool -> Printf.fprintf oc "bool";
        |Tprod(x, y) ->                                             (*product of two types*)
            p_typ oc x; Printf.fprintf oc "*" ; p_typ oc y;
        |Tarrow(l, y) ->                                            (*function type*)
                        Printf.fprintf oc "(";
                        p_ltyp oc l;                                (*list of types*)
                        Printf.fprintf oc "=>";
                        p_typ oc y;
                        Printf.fprintf oc ")";
;;

let p_arg (oc: out_channel) (arg: arg) = 
    match arg with
    |n, t -> 
             p_name oc n;                                     (*name (just a string)*)
             Printf.fprintf oc ":";
             p_typ oc t;                                      (*type*)
             Printf.fprintf oc " ";
;;

let rec p_expr oc expr = 
    match expr with                                         (*expression is either:*)
    |Econst(c) -> p_const oc c;                                 (*a constant (int or bool)*)
    |Ename(n) -> p_name oc n;                                   (*a name (string)*)
    |Eunary(uop, x) -> p_uop oc uop; p_expr oc x;               (*a unary operation and an other expression*)
    |Ebinary(bop, x, y) -> Printf.fprintf oc "("; p_expr oc x; p_bop oc bop; p_expr oc y;Printf.fprintf oc ")";         (*a binary operation and two expressions*)
    |Eif(x, y, z) ->                                            (*if 'x (expr)' then 'y (expr)' else 'z (expr)'*)
                    p_expr oc x; 
                    Printf.fprintf oc " ? ";
                    Printf.fprintf oc "(";
                    p_expr oc y;
                    Printf.fprintf oc ")";
                    Printf.fprintf oc " : ";
                    Printf.fprintf oc "(";
                    p_expr oc z;
                    Printf.fprintf oc ")";
    |Epair(x, y) ->                                             (*a pair of expressions*)
                    Printf.fprintf oc "(";
                    p_expr oc x;
                    Printf.fprintf oc ",";
                    p_expr oc y;
    |Elet(b,n,x,y) ->                                           (*let 'n (name) = 'x (expr)' in 'y (expr)'*)
                    Printf.fprintf oc "let ";
                    if b then Printf.fprintf oc "rec ";         
                    p_name oc n;
                    Printf.fprintf oc " ";
                    p_expr oc x;
                    Printf.fprintf oc " in ";
                    p_expr oc y;
    |Efun(args, expr) ->
                    Printf.fprintf oc "(";
                    List.iter (fun x -> p_arg oc x) args;       (*list of arguments of the function*)
                    Printf.fprintf oc ")";
                    Printf.fprintf oc "=>";
                    p_expr oc expr;                             (*body of the function (expression)*)
    |Eapply(x, exprs) ->
                    p_expr oc x;
                    Printf.fprintf oc " ";
                    List.iter (fun x -> p_expr oc x) exprs;     (*list of expressions*)

;;



(*let lt = Elet (false,"name" , Ename "expr1" , Ename "expr2");;
let pr = Epair ( Ename "expr1" , Ename "expr2");;
let arg1 = ("arg1", Tbool);;
let arg2 = ("arg2", Tint);;
let arg3 = ("arg3", Tprod (Tbool, Tint));;
let arg4 = ("arg4", Tarrow ([Tint; Tprod (Tbool, Tint); Tbool], Tbool));;
let func = Efun ([arg1; arg2; arg3; arg4], pr);;
p_expr stdout pr;;
p_expr stdout lt;;
p_expr stdout func;;*)