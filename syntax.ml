(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt 
type logOp = And | Or
type exp =
    | Var of id (* Var "x" --> x *)
    | ILit of int (* ILit 3 --> 3 *)
    | BLit of bool (* BLit true --> true *)
    | BinOp of binOp * exp * exp
    (* BinOp(Plus, ILit 4, Var "x") --> 4 + x *)
    | LogOp of logOp * exp * exp

    | IfExp of exp * exp * exp
    (* IfExp(BinOp(Lt, Var "x", ILit 4), 
             ILit 3, 
             Var "x") --> 
       if x<4 then 3 else x *)
    | LetExp of (id * exp) list  * exp
    | FunExp of id * exp
    | DFunExp of id * exp
    | AppExp of exp * exp
    | LetRecExp of id * id * exp * exp

and program = 
      Exp of exp
    | Decl of (id * exp) list list
    | RecDecl of id * id * exp