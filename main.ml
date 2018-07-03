open Syntax
open Eval
let string_of_binop = function
      Plus -> "Plus"
    | Lt -> "Lt"
    | Mult -> "Mult"


let string_of_logop = function
      And -> "And"
    | Or -> "Or"
(*
let rec string_of_exp =function
      Var x -> x
    | ILit i -> string_of_int i
    | BLit b -> string_of_bool b
    | BinOp (op, exp1, exp2) -> 
        "BinOp (" ^ string_of_binop op ^ ", " ^ string_of_exp exp1 ^ ", " ^ string_of_exp exp2 ^ ")"   
    | LogOp (op, exp1, exp2) ->
        "LogOp (" ^ string_of_logop op ^ ", " ^ string_of_exp exp1 ^ ", " ^ string_of_exp exp2 ^ ")"    
    | IfExp  (exp1, exp2, exp3) ->
        "IfOp ("^ string_of_exp exp1 ^ "," ^ string_of_exp exp2 ^", " ^ string_of_exp exp3 ^ ")"
    | LetExp  ((id, e) :: rest, _) ->
        "LetOp (" ^ id ^ "=" ^ string_of_exp e ^ ", "  
    | LetExp ([],exp) -> string_of_exp exp ^ ")\n"
    | LetRecExp (id, para,exp1, exp2) ->
        "LetOp (" ^ id ^ ", " ^ para ^ ", " ^ string_of_exp exp1 ^ ", " ^ string_of_exp exp2 ^ ")"  
    | FunExp  (id, exp) ->
        "FunOp (" ^ id ^ ", " ^ string_of_exp exp ^ ")"
    | DFunExp  (id, exp) ->
        "DFunOp (" ^ id ^ ", " ^ string_of_exp exp ^ ")"
    | AppExp  (exp1, exp2) ->
        "AppOp (" ^ (string_of_exp exp1) ^ ", " ^ string_of_exp exp2 ^ ")"
        
and string_of_program = function
      Exp e -> (string_of_exp e) ^ "\n"
    | Decl [] -> ""
    | Decl ([] :: rest) -> string_of_program (Decl rest)
    | Decl (((id,exp)::rem) :: rest) ->
        id ^ "=" ^ string_of_exp exp ^ "\n" ^ string_of_program (Decl (rem::rest))
    | RecDecl (id, para, exp) ->
        id ^ "=" ^ para ^ "->" ^ string_of_exp exp ^ "\n"
*)
let rec read_eval_print env =
    print_string "# ";
    flush stdout;
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    (*print_string (string_of_program decl);*)
    let newenv = eval_print env decl in

    read_eval_print newenv


let initial_env = 
    Environment.extend "i" (IntV 1)
        (Environment.extend "v" (IntV 5) 
             (Environment.extend "x" (IntV 10)
                  (Environment.extend "ii" (IntV 2)
                       (Environment.extend "iii" (IntV 3)
                            (Environment.extend "iv" (IntV 4) 
                                 (Environment.empty))))))

let _ = 
    if Array.length Sys.argv = 1 then read_eval_print initial_env
    else 
        let fname = Sys.argv.(1) in 
        
        let fin = open_in fname in
        let decl = Parser.toplevel Lexer.main (Lexing.from_channel fin) in
        ignore (eval_print initial_env decl) 