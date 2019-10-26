open Ast
exception T of string

module StringMap = Map.Make(String);;


type ttyp = 
    | Typint
    | Typbool
    | Typrod of ttyp * ttyp
    | Typfunc of ttyp list * ttyp * ttyp StringMap.t

type typenv = ttyp StringMap.t;;

let rec p_type oc t =
    match t with    
        |Typint -> Printf.fprintf oc "int";
        |Typbool -> Printf.fprintf oc "bool";
        |Typrod(t1, t2) -> p_type oc t1; Printf.fprintf oc "*"; p_type oc t2;
        |Typfunc(args, x, fenv) -> 
                    Printf.fprintf oc "(";
                    List.iter (fun x -> p_type oc x) args;
                    Printf.fprintf oc ")";
                    Printf.fprintf oc "->";
                    p_type oc x;
                    Printf.fprintf oc " (Env: ";
                    let foo k v =  Printf.fprintf oc "%s : " k; p_type oc v; Printf.fprintf oc "," in
                    StringMap.iter foo fenv;
                    Printf.fprintf oc ")\n";
;;

let p_tenv oc en = 
                let foo k v =  Printf.fprintf oc "%s : " k; p_type oc v; Printf.fprintf oc "\n" in
                StringMap.iter foo en;
;;    


let type_unary op cnst =
    match cnst with
    |Typint -> 
        begin
        match op with
            | Uineg -> Typint
            | _ -> raise (T "Wrong type\n");
        end

    |Typbool ->
        begin 
        match op with
            | Ubnot -> Typbool
            | _ -> raise (T "Wrong type\n");
        end
    |Typrod(x,y) -> 
        begin
        match op with 
            | Upfst -> x
            | Upsnd -> y
            | _ -> raise (T "Wrong type\n");

        end
    |Typfunc(_,_,_) -> raise (T "Wrong type for unary\n");
;;
let type_binary op cnst1 cnst2 = 
    match cnst1, cnst2 with
        |Typint, Typint -> 
                    begin
                    match op with
                        | Biadd -> Typint
                        | Bisub -> Typint
                        | Bimul -> Typint
                        | Bidiv -> Typint
                        | Bcleq -> Typbool;
                        | Bceq  -> Typbool;
                        | Bband -> raise (T "Wrong type: BoolAnd with integers\n");
                    end
        |Typbool, Typbool-> 
                    begin
                    match op with
                        | Bband -> Typbool;
                        | Biadd -> raise (T "Wrong type: addition with bool\n");
                        | Bisub -> raise (T "Wrong type: subtraction with bool\n");
                        | Bimul -> raise (T "Wrong type: multiplication with bool\n");
                        | Bidiv -> raise (T "Wrong type: division with bool\n");
                        | Bcleq -> raise (T "Wrong type: inequality with bool\n");
                        | Bceq  -> raise (T "Wrong type: equality with bool\n");
                    end
        |_, _ -> raise (T "Not the same type\n");
;;

let rec argtotype env arg = 
    match (snd arg) with 
        | Tint -> Typint
        | Tbool -> Typbool
        | Tprod(x,y) -> Typrod(argtotype env x, argtotype env y)
        | Tarrow(x, y) -> Typfunc(argtotypes env x, argtotype env y, env) 

and argtotypes env args =
    match args with
        |[] -> []
        |arg :: tl -> argtotype env arg :: argtotypes env tl
;;

let rec type_expr env expr = 
    print_string("############################ \n");
    print_string("Expr: ");
    p_expr stdout expr;
    print_string("\n");
    print_string("\n");
    print_string("Env: ");
    p_tenv stdout (env);
    print_string("\n");
    match expr with
        |Econst(const) ->
            begin       
            match const with
                |Cint(x) -> Typint , env;
                |Cbool(b) -> Typbool , env;
            end
        |Ename(name) -> StringMap.find name env, env;
        |Eunary(uop, x) -> let foo = type_expr env x in type_unary uop (fst foo),snd foo;
        |Ebinary(bop, x, y) ->
            let foo1 = type_expr env x in 
            let foo2 =  type_expr (snd foo1) y in
            type_binary bop (fst foo1) (fst foo2), snd foo2;
        |Eif(x, y, z) -> 
            begin
            let foo1 = (type_expr env x) in 
            match fst foo1 with 
                |Typbool -> 
                    let foo2 = (typ_expr (snd foo1) y) in
                    let foo3 = (typ_expr (snd foo1) z) in
                    if (fst foo2) = (fst foo 3) then ((fst foo2), (snd foo1)) else raise (T "Error typing ifelse\n");
                |_ -> raise (T "condition is not a boolean\n");
            end
        |Epair(x, y) -> 
            let foo1 = (type_expr env x) in
            let foo2 = (type_expr (snd foo1) y) in
            Typrod((fst foo1), (fst foo2)), snd foo2;
        |Elet (b,n,x, y) -> 
            let foo = (type_expr env x) in
            let newenv = StringMap.add n (fst foo) (snd foo) in
            let foo2 = (type_expr newenv y) in
            (fst foo2), env;
        |Efun (args, expr) -> Typfunc(args, expr, env), env;
        |Eapply (funct, expr_lst) -> 
            begin
            let rec helper env argenv args exprs i = 
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
                |Tint _ -> raise (T "Wrong type\n");
                |Tprod(_,_) -> raise (T "Wrong type\n");
                |Tbool _ -> raise (T "Wrong type\n");
                |Tname _ -> raise (T "Wrong type\n");
            end
;;
