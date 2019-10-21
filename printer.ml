open Ast

let p_name oc (name: string) =
    Printf.fprintf oc "%s" name;;

let p_const oc const = match const with
                    |Cint(x) -> Printf.fprintf oc "%d" x;
                    |Cbool(b) -> Printf.fprintf oc "%b" b;
;;
let p_uop oc uop = match uop with
                  |Uineg -> Printf.fprintf oc "-";
                  |Ubnot -> Printf.fprintf oc "!";
                  |Upfst -> Printf.fprintf oc "first";
                  |Upsnd -> Printf.fprintf oc "second";
;;
let p_bop oc bop = match bop with
                  |Biadd -> Printf.fprintf oc "+";
                  |Bisub -> Printf.fprintf oc "-";
                  |Bimul -> Printf.fprintf oc "*";
                  |Bidiv -> Printf.fprintf oc "/";
                  |Bband -> Printf.fprintf oc "&";
                  |Bcleq -> Printf.fprintf oc "<=";
                  |Bceq -> Printf.fprintf oc "==";
;;

let rec p_typ oc (typ: typ) = 
    let rec p_ltyp oc ltyp = match ltyp with
        | [] -> Printf.fprintf oc "";
        | h::b -> begin
                    p_typ oc h;
                    Printf.fprintf oc " ";
                    p_ltyp oc b;
                end;
    in
    match typ with
        |Tint -> Printf.fprintf oc "int";
        |Tbool -> Printf.fprintf oc "bool";
        |Tprod(x, y) ->
            p_typ oc x; Printf.fprintf oc "*" ; p_typ oc y;
        |Tarrow(l, y) ->
                        Printf.fprintf oc "(";
                        p_ltyp oc l;
                        Printf.fprintf oc "=>";
                        p_typ oc y;
                        Printf.fprintf oc ")";
;;

let p_arg (oc: out_channel) (arg: arg) = 
    match arg with
    |n, t -> 
             p_name oc n;
             Printf.fprintf oc ":";
             p_typ oc t;
             Printf.fprintf oc " ";
;;

let rec p_expr oc expr = 
    match expr with
    |Econst(c) -> p_const oc c; 
    |Ename(n) -> p_name oc n;
    |Eunary(uop, x) -> p_uop oc uop; p_expr oc x;
    |Ebinary(bop, x, y) -> Printf.fprintf oc "("; p_expr oc x; p_bop oc bop; p_expr oc y;Printf.fprintf oc ")";
    |Eif(x, y, z) ->
                    p_expr oc x; 
                    Printf.fprintf oc " ? ";
                    Printf.fprintf oc "(";
                    p_expr oc y;
                    Printf.fprintf oc ")";
                    Printf.fprintf oc " : ";
                    Printf.fprintf oc "(";
                    p_expr oc z;
                    Printf.fprintf oc ")";
    |Epair(x, y) ->
                    Printf.fprintf oc "(";
                    p_expr oc x;
                    Printf.fprintf oc ",";
                    p_expr oc y;
    |Elet(b,n,x,y) ->
                    Printf.fprintf oc "let ";
                    if b then Printf.fprintf oc "rec ";
                    p_name oc n;
                    Printf.fprintf oc " ";
                    p_expr oc x;
                    Printf.fprintf oc " in ";
                    p_expr oc y;
    |Efun(args, expr) ->
                    Printf.fprintf oc "(";
                    List.iter (fun x -> p_arg oc x) args;
                    Printf.fprintf oc ")";
                    Printf.fprintf oc "=>";
                    p_expr oc expr;
    |Eapply(x, exprs) ->
                    p_expr oc x;
                    Printf.fprintf oc " ";
                    List.iter (fun x -> p_expr oc x) exprs;

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