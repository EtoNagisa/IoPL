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


DECL :
    LET x=NAME EQ e=Expr n=ANDDECL d=DECL { Let ((x, e) :: n) :: d }  
  | LET x=NAME e=EQFun n=ANDDECL d=DECL { Let ((x, e) :: n) :: d }
  | LET REC x=NAME e=EQFun n=ANDDECL d=DECL { Letrec((x, e) :: n) :: d }
  | LET REC x=NAME EQ e=FunExpr n=ANDDECL d=DECL { Letrec((x, e) :: n) :: d }
  |  {[]}

ANDDECL :
    AND x=NAME EQ e=Expr n=ANDDECL { (x, e)::n }
  | AND x=NAME e=EQFun n=ANDDECL { (x, e)::n }
  | {[]}
Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=ORExpr { e }
  | e=FunExpr { e }

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
  | i=NAME   { Var i }
  | LPAREN e=Expr RPAREN { e }

NAME :
    LPAREN PLUS RPAREN { "( + )" }
  | LPAREN MULT RPAREN { "( * )" }
  | LPAREN LT RPAREN { "( < )" }
  | LPAREN LAND RPAREN { "( && )"}
  | LPAREN LOR RPAREN { "( || )"}
  | x=ID { x }
  
IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetExpr :
    LET x=NAME EQ e=Expr n=ANDDECL IN ex=Expr { LetExp (Let ((x, e)::n), ex) }
  | LET x=NAME e=EQFun n=ANDDECL IN ex=Expr { LetExp(Let ((x, e)::n), ex) }
  | LET REC x=NAME e=EQFun n=ANDDECL IN ex=Expr { LetExp (Letrec ((x, e)::n), ex) }
  | LET REC x=NAME EQ e=FunExpr n=ANDDECL IN ex=Expr { LetExp (Letrec ((x, e)::n), ex) }

FunExpr :
    FUN e=ARFun { e }
  | DFUN e=DARFun { e }

EQFun :
    x=NAME e=EQFun { FunExp (x, e) }
  | x=NAME EQ e=Expr { FunExp (x, e) }

ARFun :
    x=NAME e=ARFun { FunExp (x, e) }
  | x=NAME RARROW e=Expr { FunExp (x, e) }

DARFun :
    x=NAME e=DARFun { DFunExp (x, e) }
  | x=NAME RARROW e=Expr { DFunExp (x, e) }
    