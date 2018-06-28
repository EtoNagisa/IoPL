open Syntax 

(* 値の定義 *)
type exval =
    | IntV of int
    | BoolV of bool
    | ProcV of id * exp * dnval Environment.t ref
and dnval = exval

(* exval は式を評価して得られる値．dnval は変数と紐付けられる値．今回
   の言語ではこの両者は同じになるが，この2つが異なる言語もある．教科書
   参照． *)

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
      IntV i -> string_of_int i
    | BoolV b -> string_of_bool b
    | ProcV _ -> "<fun>"

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
      Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
    | Plus, _, _ -> err ("Both arguments must be integer: +")
    | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
    | Mult, _, _ -> err ("Both arguments must be integer: *")
    | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
    | Lt, _, _ -> err ("Both arguments must be integer: <")


let rec eval_exp env = function
      Var x -> 
        (try Environment.lookup x env with 
           Environment.Not_bound -> err ("Variable not bound: " ^ x))
    | ILit i -> IntV i
    | BLit b -> BoolV b
    | BinOp (op, exp1, exp2) -> 
        let arg1 = eval_exp env exp1 in
        let arg2 = eval_exp env exp2 in
        apply_prim op arg1 arg2
    | LogOp (op, exp1, exp2) ->
        let arg1 = eval_exp env exp1 in
        (match op, arg1 with
           And, BoolV false -> BoolV false
         | And, BoolV true -> 
             let arg2 = eval_exp env exp2 in
             (match arg2 with 
                BoolV b2 -> BoolV b2
              | _ -> err ("The right argument must be bool: && "))
         | And, _ -> err("Both arguments must be bool:  &&")
         | Or, BoolV true -> BoolV true
         | Or, BoolV false ->
             let arg2 = eval_exp env exp2 in
             (match arg2 with
                BoolV b2 -> BoolV b2
              | _ -> err ("The right argument must be bool: || "))
         | Or, _ -> err("Both arguments must be bool: ||"))
    | IfExp (exp1, exp2, exp3) ->
        let test = eval_exp env exp1 in
        (match test with
           BoolV true -> eval_exp env exp2 
         | BoolV false -> eval_exp env exp3
         | _ -> err ("Test expression must be boolean: if"))
    | LetExp (id, exp1, exp2) ->
        let value = eval_exp env exp1 in
        eval_exp (Environment.extend id value env) exp2
    | FunExp (id, exp) -> ProcV(id, exp, ref env)
    | AppExp (exp1, exp2) ->
        let funval = eval_exp env exp1 in
        let arg = eval_exp env exp2 in
        (match funval with
           ProcV (id, body, env') ->
             let newenv = Environment.extend id arg !env' in
             eval_exp newenv body
         | _ -> err ("Non-function value is applied"))
    | LetRecExp  (id, para, exp1, exp2) ->
        let dummyenv = ref Environment.empty in
        let newenv = 
            Environment.extend id (ProcV (para, exp1, dummyenv)) env in
        dummyenv :=newenv;
        eval_exp newenv exp2

let rec exists x = function
      [] -> false
    | (id, _) :: rest -> 
        if id=x then true else exists x rest

let rec print_distinct = function
      [] -> ()
    | (id, v) :: rest ->
        if exists id rest then  print_distinct rest
        else (
            print_string ("val " ^ id ^ " = " ^ string_of_exval v ^ "\n");
            print_distinct rest
        )
let rec eval_decl env = function
      [] -> (env, [])
    | (id, e) :: rest ->
        (try let v = eval_exp env e in
             let (retenv, reststr) = eval_decl (Environment.extend id v env) rest in
             (retenv, (id, v)::reststr)
         with
           Error s ->
             print_string s;
             print_newline();
             (env, [])
        )

let eval_print env = function
      Exp e -> 
        (try let v = eval_exp env e in 
             print_string "val - = ";
             pp_val v;
             print_newline ();
             env
         with
           Error s -> 
             print_string s;
             print_newline();
             env
        )
    | Decl l ->
        let (newenv,l) = eval_decl env l in
        print_distinct l;
        newenv
    | RecDecl (id, para, e) ->
        (try
             let dummyenv = ref Environment.empty in
             let newenv =
                 Environment.extend id (ProcV (para, e, dummyenv)) env in
             dummyenv := newenv;
             print_string ("val " ^ id ^ " = ");
             pp_val (ProcV (para, e, dummyenv));
             print_newline();
             newenv
         with
           Error s -> 
             print_string s;
             print_newline();
             env;
        )


