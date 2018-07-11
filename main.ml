open Syntax
open Eval
open Typing

let rec print_val l ty = 
    match l,ty with
    ([], []) -> ()
|   ((id, v) :: restl , (_, t) :: restty)-> 
        print_string ("val " ^ id ^ " : ");
        pp_ty t;
        print_string " = ";
        pp_val v;
        print_newline();
        print_val restl restty
let rec read_eval_print env tyenv=
    print_string "# ";
    flush stdout;
    try
        let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
        let (newenv, l)  = eval env decl in
        let ty = ty_decl tyenv decl in
        (print_val l ty;
        read_eval_print newenv tyenv)
    with
        Eval.Error s ->
            print_string s;
            print_newline();
            read_eval_print env tyenv
    |   Failure s ->
            print_string s;
            print_newline();
            read_eval_print env tyenv
    |   Typing.Error s ->
            print_string s;
            print_newline();
            read_eval_print env tyenv
    |   _ ->
            print_string "an error has occured while parsing";
            print_newline();
            read_eval_print env tyenv

let initial_env =
    Environment.extend "i" (IntV 1)
        (Environment.extend "v" (IntV 5)
            (Environment.extend "x" (IntV 10) 
                (Environment.extend "( + )" (ProcV ("x", FunExp ("y", BinOp (Plus, Var "x", Var "y")), ref Environment.empty)) 
                    (Environment.extend "( * )" (ProcV ("x", FunExp ("y", BinOp (Mult, Var "x", Var "y")), ref Environment.empty)) 
                        (Environment.extend "( + )" (ProcV ("x", FunExp ("y", BinOp (Lt, Var "x", Var "y")), ref Environment.empty)) 
                            (Environment.extend "( && )" (ProcV ("x", FunExp ("y", BinOp (And, Var "x", Var "y")), ref Environment.empty)) 
                                (Environment.extend "( || )" (ProcV ("x", FunExp ("y", BinOp (Or, Var "x", Var "y")), ref Environment.empty)) 
                                    Environment.empty)))))))

let initial_tyenv =
    Environment.extend "i" TyInt
        (Environment.extend "v" TyInt
            (Environment.extend "x" TyInt Environment.empty))
let batch_interpreter () =
    let fname = Sys.argv.(1) in 
        try
            let fin = open_in fname in
            let decl = Parser.toplevel Lexer.main (Lexing.from_channel fin) in
            let (_, l) = eval initial_env decl in
            (print_val l;
            close_in fin)
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
    if Array.length Sys.argv = 1 then read_eval_print initial_env initial_tyenv
    else batch_interpreter ()