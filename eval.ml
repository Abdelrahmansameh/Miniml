open Ast
open Printer
exception T of string

module StringMap = Map.Make(String);;

(*Target value types*)
type t = 
    | Tint of int
    | Tbool of bool
    | Tprod of t * t
    | Tfunc of arg list * expr * t StringMap.t (*Functions hold the environment in which they were defined*)

(*Store the value of variables*)
type env = t StringMap.t;;

(*print function*)
let rec p_t oc t =
    match t with    
        |Tint(x) -> Printf.fprintf oc "%d" x;
        |Tbool(b) -> Printf.fprintf oc "%b" b;
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
(*print function*)
let p_env oc en = 
                let foo k v =  Printf.fprintf oc "%s : " k; p_t oc v; Printf.fprintf oc "\n" in
                StringMap.iter foo en;
;;    


let eval_unary op cnst =
    match cnst with
        |Tint(x) -> Tint (-x);                                          (*negation for integers*)
        |Tbool(b) -> Tbool (not b);                                     (*negation for booleans*)
        |Tfunc(_,_,_) -> raise (T "Wrong type for unary\n");     
        |Tprod(frst,scnd) -> 
            match op with
                |Upfst -> frst;
                |Upsnd -> scnd;
                |_ -> raise (T "Wrong type for unary\n");
;; 

let eval_binary op cnst1 cnst2 = 
    match cnst1, cnst2 with
        |Tint(x1), Tint(x2) -> 
                    begin
                    match op with
                        | Bband -> raise (T "Wrong type: BoolAnd with integers\n");         (*boolean and, not for int*)
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
                        | Biadd -> raise (T "Wrong type: addition with bool\n");
                        | Bisub -> raise (T "Wrong type: subtraction with bool\n");
                        | Bimul -> raise (T "Wrong type: multiplication with bool\n");
                        | Bidiv -> raise (T "Wrong type: division with bool\n");
                        | Bcleq -> raise (T "Wrong type: inequality with bool\n");
                        | Bceq  -> raise (T "Wrong type: equality with bool\n");
                    end
        |Tbool(_), Tint(_) -> raise (T "Not the same type\n");
        |_, _ -> raise (T "Wrong type for binop\n");                 (*all other cases (Tprod and Tfunc) are wrong types for a binop operation*)
;;


let rec eval_expr env expr = 
    (*
    Evaluate the expression, returns the new 
    environment after evaluation, and the value 
    *)
    match expr with
        |Econst(const) ->
            begin       
            match const with
                |Cint(x) -> Tint(x), env;
                |Cbool(b) -> Tbool(b), env;
            end
        |Ename(name) -> StringMap.find name env, env;
        |Eunary(uop, x) -> 
            let foo = eval_expr env x in 
            eval_unary uop (fst foo),snd foo;
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
                |Tint _ -> raise (T "condition is not a boolean\n");
                |Tprod(_,_) -> raise (T "condition is not a boolean\n");
                |Tfunc(_,_,_) -> raise (T "condition is not a boolean\n");
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
            let rec helper env argenv args exprs i = 
                (*Iterates over the arguments lists from the call
                and the definition simultaneously and adds
                them to the environment where the expression
                of the function will be evaluated *)
                match i with
                |(-1) -> argenv, (-1); 
                |x ->
                    let foo = eval_expr env (List.nth exprs i) in
                    helper env (StringMap.add (fst (List.nth args i) ) (fst foo) argenv ) args exprs (i-1);
            in
            let foo = eval_expr env funct in
            match fst foo with
                |Tfunc(arg_lst, expr, fenv) -> 
                    let wrapper_helper env args exprs  = helper env fenv args exprs ((List.length args) -1 ) in 
                    let argenv = fst (wrapper_helper (snd foo) arg_lst expr_lst ) in 
                    let foo2 = eval_expr argenv expr in
                    fst foo2, snd foo;
                |Tint _ -> raise (T "Argument is not callable\n");
                |Tprod(_,_) -> raise (T "Argument is not callable\n");
                |Tbool _ -> raise (T "Argument is not callable\n");
            end
;;
