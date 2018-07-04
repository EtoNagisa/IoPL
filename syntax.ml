(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | And | Or

type letseq = Let of (id * exp) list | Letrec of (id * exp) list
and exp =
    | Var of id
    | ILit of int
    | BLit of bool
    | BinOp of binOp * exp * exp
    | IfExp of exp * exp * exp
    | LetExp of letseq * exp
    | FunExp of id * exp
    | DFunExp of id * exp
    | AppExp of exp * exp

and program = 
      Exp of exp
    | Decl of letseq list