(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | And | Or | Cons

type letseq = Let of (id * exp) list | Letrec of (id * exp) list

and exp =
    | Var of id
    | ILit of int
    | BLit of bool
    | ListExp of exp list
    | BinOp of binOp * exp * exp
    | IfExp of exp * exp * exp
    | LetExp of letseq * exp
    | FunExp of id * exp
    | DFunExp of id * exp
    | AppExp of exp * exp
    | MatchExp of exp * exp * id * id * exp

and program = 
      Exp of exp
    | Decl of letseq list

type tyvar = int

type ty = 
    TyInt
|   TyBool
|   TyVar of tyvar
|   TyFun of ty * ty
|   TyList of ty