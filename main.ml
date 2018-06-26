open Syntax
open Eval
let string_of_binop = function
    Plus -> "Plus"
  | Lt -> "Lt"
  | Mult -> "Mult"

  
 let string_of_logop = function
    And -> "And"
  | Or -> "Or"
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
      | LetExp  (id, exp1, exp2) ->
          "LetOp (" ^ id ^ ", " ^ string_of_exp exp1 ^ ", " ^ string_of_exp exp2 ^ ")"  
      | LetRecExp (id, para,exp1, exp2) ->
          "LetOp (" ^ id ^ ", " ^ para ^ ", " ^ string_of_exp exp1 ^ ", " ^ string_of_exp exp2 ^ ")"  
      | FunExp  (id, exp1) ->
          "FunOp (" ^ id ^ ", " ^ string_of_exp exp1 ^ ")"
      | AppExp  (exp1, exp2) ->
          "AppOp (" ^ (string_of_exp exp1) ^ ", " ^ string_of_exp exp2 ^ ")"
let rec string_of_program = function
    Exp e -> (string_of_exp e) ^ "\n"
  | Decl ((id, exp) :: rest) ->
      id ^ "=" ^ string_of_exp exp ^ "\n" ^ string_of_program (Decl rest)
  | Decl _ -> 
      ""
  | RecDecl (id, para, exp) ->
      id ^ "=" ^ para ^ "->" ^ string_of_exp exp ^ "\n"
      
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

let _ = read_eval_print initial_env
