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
      | FunExp  (id, exp1) ->
          "FunOp (" ^ id ^ ", " ^ string_of_exp exp1 ^ ")"
      | AppExp  (exp1, exp2) ->
          "AppOp (" ^ (string_of_exp exp1) ^ ", " ^ string_of_exp exp2 ^ ")"
let string_of_program = function
    Exp e -> (string_of_exp e) ^ "\n"
  | Decl (id, exp) ->
      (id ^ "=" ^string_of_exp exp) ^ "\n"
let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try(
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (id, newenv, v) = eval_decl env decl in
    (*print_string (string_of_program decl);*)
    Printf.printf "val %s = " id;
    pp_val v;
    print_newline();
      
    read_eval_print newenv
  )
  with 
    Error s ->
      print_string s;
      print_newline();
      read_eval_print env
  | Failure s ->
      print_string s;
      print_newline();
      read_eval_print env
  | _ ->
      print_string "Parser.MenhirBasics.Error";
      print_newline();
      read_eval_print env
      
let initial_env = 
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5) 
      (Environment.extend "x" (IntV 10)
        (Environment.extend "ii" (IntV 2)
          (Environment.extend "iii" (IntV 3)
            (Environment.extend "iv" (IntV 4) 
              (Environment.empty))))))

let _ = read_eval_print initial_env
