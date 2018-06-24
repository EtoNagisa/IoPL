%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }

Expr :
    e=IfExpr { e }
  | e=LTExpr { e }

LTExpr : 
    l=ORExpr LT r=ORExpr { BinOp (Lt, l, r) }
  | e=ORExpr { e }

ORExpr :
    l=ORExpr OR r=ANDExpr { LogOp (OR, l, r) }
  | e=ANDExpr { e }

ANDExpr :
    l=ANDExpr AND r=PExpr { LogOp (AND, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr : 
    l=MExpr MULT r=AExpr { BinOp (Mult, l, r) }
  | e=AExpr { e }
  

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
