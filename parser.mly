%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT LAND LOR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token RARROW FUN REC AND DFUN

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | e=DECL SEMISEMI { Decl e }
  | LET REC x=ID EQ FUN p=ID RARROW e=Expr SEMISEMI { RecDecl(x, p, e) }

DECL :
    LET x=ID EQ e=Expr n=ANDDECL d=DECL { ((x, e)::n) :: d }  
  | LET x=ID e=EQFun n=ANDDECL d=DECL { ((x, e)::n) :: d }
  |  {[]}
ANDDECL :
    AND x=ID EQ e=Expr n=ANDDECL { (x, e)::n }
  | AND x=ID e=EQFun n=ANDDECL { (x, e)::n }
  | {[]}
Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=LetRecExpr { e }
  | e=ORExpr { e }
  | e=FunExpr { e }
  | e=DFunExpr { e }

ORExpr :
    l=ANDExpr LOR r=ORExpr { LogOp (Or, l, r) }
  | e=ANDExpr { e }

ANDExpr :
    l=LTExpr LAND r=ANDExpr { LogOp (And, l, r) }
  | e=LTExpr { e }

LTExpr : 
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr : 
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }
  
AppExpr :
    e1=AppExpr e2=AExpr {AppExp (e1, e2) }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetExpr :
    LET x=ID EQ e=Expr n=ANDDECL IN ex=Expr { LetExp ((x, e)::n, ex) }
  | LET x=ID e=EQFun n=ANDDECL IN ex=Expr { LetExp((x, e)::n, ex) }

LetRecExpr :
    LET REC x=ID EQ FUN p=ID RARROW e1=Expr IN e2=Expr {LetRecExp (x, p, e1, e2) }

FunExpr :
    FUN e=ARFun { e }

DFunExpr :
    DFUN e=DARFun { e }

EQFun :
    x=ID e=EQFun { FunExp (x, e) }
  | x=ID EQ e=Expr { FunExp (x, e) }

ARFun :
    x=ID e=ARFun { FunExp (x, e) }
  | x=ID RARROW e=Expr { FunExp (x, e) }

DARFun :
    x=ID e=DARFun { DFunExp (x, e) }
  | x=ID RARROW e=Expr { DFunExp (x, e) }
    