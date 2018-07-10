open Syntax 

(* 値の定義 *)
type exval =
    | IntV of int
    | BoolV of bool
    | ProcV of id * exp * dnval Environment.t ref
    | DProcV of id * exp 
and dnval = exval

(* exval は式を評価して得られる値．dnval は変数と紐付けられる値．今回
   の言語ではこの両者は同じになるが，この2つが異なる言語もある．教科書
   参照． *)

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
|   BoolV b -> string_of_bool b
|   ProcV _ -> "<fun>"
|   DProcV _ -> "<fun>"

let pp_val v = print_string (string_of_exval v)


let rec exists x = function
    [] -> false
|   (id, _) :: rest -> 
        if id=x then true else exists x rest
let rec consistent = function
    [] -> true
|   (id, _) :: rest ->
        if exists id rest then false else consistent rest  

let rec distinct l ty =
    match l,ty with
        ([], []) -> ([], [])
    |   ((id, v) :: restl,t :: restty) ->
        if exists id restl then distinct restl restty
        else 
            let (a,b) = distinct restl restty in
            ((id, v ):: a, t::b)
        
let rec apply_prim env op arg1 arg2 = match op, arg1, arg2 with
    And, _, _ ->
        let e1 = eval_exp env arg1 in
        (match e1 with
            BoolV false -> BoolV false
        |   BoolV true ->
                let e2 = eval_exp env arg2 in
                (match e2 with
                    BoolV b1 -> BoolV b1
                |   _ -> err ("The right argument must be bool: && "))
        |   _ -> err("Both arguments must be bool:  &&"))
|   Or, _, _ ->
        let e1 = eval_exp env arg1 in
        (match e1 with
            BoolV true -> BoolV true
        |   BoolV false ->
                let e2 = eval_exp env arg2 in
                (match e2 with
                    BoolV b1 -> BoolV b1
                |   _ -> err ("The right argument must be bool: || "))
        |   _ -> err("Both arguments must be bool:  ||"))
|   _ ->
        let e1 = eval_exp env arg1 in
        let e2 = eval_exp env arg2 in
        
        match op, e1, e2 with
            Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
        |   Plus, _, _ -> err ("Both arguments must be integer: +")
        |   Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
        |   Mult, _, _ -> err ("Both arguments must be integer: *")
        |   Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
        |   Lt, _, _ -> err ("Both arguments must be integer: <")

and eval_exp env = function
    Var x -> 
        (try Environment.lookup x env with 
           Environment.Not_bound -> err ("Variable not bound: " ^ x))
|   ILit i -> IntV i
|   BLit b -> BoolV b
|   BinOp (op, exp1, exp2) -> 
        apply_prim env op exp1 exp2
|   IfExp (exp1, exp2, exp3) ->
        let test = eval_exp env exp1 in
        (match test with
           BoolV true -> eval_exp env exp2 
         | BoolV false -> eval_exp env exp3
         | _ -> err ("Test expression must be boolean: if"))
|   LetExp (l, exp) ->
        (match l with 
            Let li ->
                let (newenv, _) = eval_decls env [l] in
                eval_exp newenv exp
        |   Letrec li ->
                let (newenv, _) = eval_decls env [l] in
                eval_exp newenv exp)
|   FunExp (id, exp) -> ProcV(id, exp, ref env)
|   DFunExp (id, exp) -> DProcV(id, exp)
|   AppExp (exp1, exp2) ->
        let funval = eval_exp env exp1 in
        let arg = eval_exp env exp2 in
        (match funval with
          
            ProcV (id, body, env') ->
                let newenv = Environment.extend id arg !env' in
                eval_exp newenv body
        |   DProcV (id, body) ->
                let newenv = Environment.extend id arg env in
                eval_exp newenv body
        |   _ -> err ("Non-function value is applied"))

and eval_decl env renv = function
    [] -> (renv, [])
|   (id, e) :: rest ->
        let v = eval_exp env e in
        let (retenv, reststr) = eval_decl env (Environment.extend id v renv) rest in
        if consistent ((id,v)::reststr) then (retenv, (id, v)::reststr)
        else err ("variable " ^ id ^ " is bound several times")
and eval_rec_decl env l = 
    let dummyenv = ref Environment.empty in
    let rec f env = function
        [] -> (env, [])
    |   (id, FunExp(x, e)) :: rest ->
            let v = ProcV (x, e, dummyenv) in
            let newenv = Environment.extend id v env in 
            let (retenv,reststr) = f (Environment.extend id v newenv) rest in
            if consistent ((id,v)::reststr) then (retenv, (id, v)::reststr)
            else err ("variable " ^ id ^ " is bound several times")
    |   _ ->
            err ("only function can be decelared")
    in
    let (retenv,retstr) = f env l in
    dummyenv := retenv;
    (retenv,retstr)
            

and eval_decls env = function
    [] -> (env,[])
|   (Let l ):: rest ->
        let (newenv,str) = eval_decl env env l in
        let (retenv,retstr) = eval_decls newenv rest
        in (retenv,str@retstr)
|   (Letrec l) :: rest ->
        let (newenv,str) = eval_rec_decl env l in
        let (retenv,retstr) = eval_decls newenv rest
        in (retenv,str@retstr)            

let eval env = function
    Exp e -> 
        let v = eval_exp env e in 
        (env, ["-", v])
|   Decl d ->
        eval_decls env d
        

