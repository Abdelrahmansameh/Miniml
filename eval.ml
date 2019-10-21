open Ast
open Printer
exception T of string

module StringMap = Map.Make(String);;


type t = 
    | Tint of int
    | Tbool of bool
    | Tprod of t * t
    | Tfunc of arg list * expr * t StringMap.t
    | Tname of string

type env = t StringMap.t;;

let rec p_t oc t =
    match t with    
        |Tint(x) -> Printf.fprintf oc "%d" x;
        |Tbool(b) -> Printf.fprintf oc "%b" b;
        |Tname(name) -> Printf.fprintf oc "%s" name;
        |Tprod(t1, t2) -> p_t oc t1; Printf.fprintf oc "*"; p_t oc t2;
        |Tfunc(args, x, fenv) -> 
                    Printf.fprintf oc "(";
                    List.iter (fun x -> p_arg oc x) args;
                    Printf.fprintf oc ")";
                    Printf.fprintf oc "->";
                    p_expr oc x;
                    Printf.fprintf oc " (Env: ";
                    let foo k v =  Printf.fprintf oc "%s : " k; p_t oc v; Printf.fprintf oc "," in
                    StringMap.iter foo fenv;
                    Printf.fprintf oc ")\n";
;;

let p_env oc en = 
                let foo k v =  Printf.fprintf oc "%s : " k; p_t oc v; Printf.fprintf oc "\n" in
                StringMap.iter foo en;
;;    


let eval_unary op cnst =
    match cnst with
        |Tint(x) -> Tint (-x);
        |Tbool(b) -> Tbool (not b);

;;
let eval_binary op cnst1 cnst2 = 
    match cnst1, cnst2 with
        |Tint(x1), Tint(x2) -> 
                    begin
                    match op with
                        | Bband -> raise (T "Wrong type\n");
                        | Biadd -> Tint(x1 + x2);
                        | Bisub -> Tint(x1 - x2);
                        | Bimul -> Tint(x1 * x2);
                        | Bidiv -> Tint(x1 / x2);
                        | Bcleq -> Tbool(x1 <= x2);
                        | Bceq  -> Tbool( x1 == x2);
                    end
        |Tbool(b1), Tbool(b2) -> 
                    begin
                    match op with
                        | Bband -> Tbool( b1 && b2);
                        | Biadd -> raise (T "Wrong type\n");
                        | Bisub -> raise (T "Wrong type\n");
                        | Bimul -> raise (T "Wrong type\n");
                        | Bidiv -> raise (T "Wrong type\n");
                        | Bcleq -> raise (T "Wrong type\n");
                        | Bceq  -> raise (T "Wrong type\n");
                    end
        |Tbool(_), Tint(_) -> raise (T "Wrong type\n");
        |Tint(_), Tbool(_) -> raise (T "Wrong type\n");
;;


let rec eval_expr env expr = 
    print_string("############################ \n");
    print_string("Expr: ");
    p_expr stdout expr;
    print_string("\n");
    print_string("\n");
    print_string("Env: ");
    p_env stdout (env);
    print_string("\n");
    match expr with
        |Econst(const) ->
            begin       
            match const with
                |Cint(x) -> Tint(x), env;
                |Cbool(b) -> Tbool(b), env;
            end
        |Ename(name) -> StringMap.find name env, env;
        |Eunary(uop, x) -> let foo = eval_expr env x in eval_unary uop (fst foo),snd foo;
        |Ebinary(bop, x, y) ->
            let foo1 = eval_expr env x in 
            let foo2 =  eval_expr (snd foo1) y in
            eval_binary bop (fst foo1) (fst foo2), snd foo2;
        |Eif(x, y, z) -> 
            begin
            let foo1 = (eval_expr env x) in 
            match fst foo1 with 
                |Tbool(b) -> 
                    if b then (eval_expr (snd foo1) y) else (eval_expr (snd foo1) z);
            end
        |Epair(x, y) -> 
            let foo1 = (eval_expr env x) in
            let foo2 = (eval_expr (snd foo1) y) in
            Tprod((fst foo1), (fst foo2)), snd foo2;
        |Elet (b,n,x, y) -> 
            let foo = (eval_expr env x) in
            let newenv = StringMap.add n (fst foo) (snd foo) in
            let foo2 = (eval_expr newenv y) in
            (fst foo2), env;
        |Efun (args, expr) -> Tfunc(args, expr, env), env;
        |Eapply (funct, expr_lst) -> 
            begin
            let rec helper env args exprs i = 
                match i with
                |(-1) -> env, (-1);
                |x ->
                    let foo = eval_expr env (List.nth exprs i) in
                    helper (StringMap.add (fst (List.nth args i) ) (fst foo) (snd foo) ) args exprs (i-1);
            in
            let wrapper_helper env args exprs  = helper env  args exprs ((List.length args) -1 ) in 
            let foo = eval_expr env funct in
            match fst foo with
                |Tfunc(arg_lst, expr, fenv) -> 
                    let newenv = fst (wrapper_helper (snd foo) arg_lst expr_lst ) in 
                    
                    let foo2 = eval_expr newenv expr in
                    fst foo2, snd foo2;
            end
;;