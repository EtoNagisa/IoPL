open Syntax
open Eval

let rec read_eval_print env =
    print_string "# ";
    flush stdout;
    try
        let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
        let newenv = eval_print env decl in
        read_eval_print newenv
    with
        Error s ->
            print_string s;
            print_newline();
            read_eval_print env
    |   Failure s ->
            print_string s;
            print_newline();
            read_eval_print env
    |   _ ->
            print_string "an error has occured while parsing";
            print_newline();
            read_eval_print env

let initial_env = 
    Environment.extend "( + )" (ProcV ("x", FunExp ("y", BinOp (Plus, Var "x", Var "y")), ref Environment.empty)) 
        (Environment.extend "( * )" (ProcV ("x", FunExp ("y", BinOp (Mult, Var "x", Var "y")), ref Environment.empty)) 
            (Environment.extend "( + )" (ProcV ("x", FunExp ("y", BinOp (Lt, Var "x", Var "y")), ref Environment.empty)) 
                (Environment.extend "( && )" (ProcV ("x", FunExp ("y", BinOp (And, Var "x", Var "y")), ref Environment.empty)) 
                    (Environment.extend "( || )" (ProcV ("x", FunExp ("y", BinOp (Or, Var "x", Var "y")), ref Environment.empty)) 
                        Environment.empty))))

let batch_interpreter () =
    let fname = Sys.argv.(1) in 
        try
            let fin = open_in fname in
            let decl = Parser.toplevel Lexer.main (Lexing.from_channel fin) in
            ignore(eval_print initial_env decl);
            close_in fin
        with   
            Error s ->
                print_string s;
                print_newline()
        |   Failure s ->
                print_string s;
                print_newline()
        |   Sys_error s ->
                print_string s;
                print_newline()
        |   _ ->
                print_string "an error has occured while parsing";
                print_newline()

                        
let _ = 
    if Array.length Sys.argv = 1 then read_eval_print initial_env
    else batch_interpreter ()