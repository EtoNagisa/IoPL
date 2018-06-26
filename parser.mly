%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token RARROW FUN REC

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
    LET x=ID EQ e=Expr n=DECL { ((x, e)::n)}
  |  {[]}

Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=LetRecExpr { e }
  | e=ORExpr { e }
  | e=FunExpr { e }

ORExpr :
    l=ANDExpr OR r=ORExpr { LogOp (Or, l, r) }
  | e=ANDExpr { e }

ANDExpr :
    l=LTExpr AND r=ANDExpr { LogOp (And, l, r) }
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
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }

LetRecExpr :
    LET REC x=ID EQ FUN p=ID RARROW e1=Expr IN e2=Expr {LetRecExp (x, p, e1, e2) }

FunExpr :
    FUN x=ID RARROW e=Expr { FunExp (x, e) }