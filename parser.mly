%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT LAND LOR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ 
%token RARROW FUN REC AND DFUN
%token LBRACKET RBRACKET CONS MATCH WITH PIPE
%token SEMI

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | e=Decl SEMISEMI { Decl e }


Decl :
    LET x=Name EQ e=Expr n=AndDecl d=Decl { Let ((x, e) :: n) :: d }  
  | LET x=Name e=EQFun n=AndDecl d=Decl { Let ((x, e) :: n) :: d }
  | LET REC x=Name e=EQFun n=AndDecl d=Decl { Letrec((x, e) :: n) :: d }
  | LET REC x=Name EQ e=FunExpr n=AndDecl d=Decl { Letrec((x, e) :: n) :: d }
  |  {[]}

AndDecl :
    AND x=Name EQ e=Expr n=AndDecl { (x, e)::n }
  | AND x=Name e=EQFun n=AndDecl { (x, e)::n }
  | {[]}
Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=FunExpr { e }
  | e=MatchExpr { e }
  | e=ORExpr { e }


ORExpr :
    l=ANDExpr LOR r=ORExpr { BinOp (Or, l, r) }
  | e=ANDExpr { e }

ANDExpr :
    l=CONSExpr LAND r=ANDExpr { BinOp (And, l, r) }
  | e=CONSExpr { e }

CONSExpr :
    l=LTExpr CONS r=CONSExpr { BinOp (Cons, l, r) }
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
  | i=Name   { Var i }
  | LBRACKET e=Exprs RBRACKET { ListExp e }
  | LPAREN e=Expr RPAREN { e }

Name :
    LPAREN PLUS RPAREN { "( + )" }
  | LPAREN MULT RPAREN { "( * )" }
  | LPAREN LT RPAREN { "( < )" }
  | LPAREN LAND RPAREN { "( && )"}
  | LPAREN LOR RPAREN { "( || )"}
  | x=ID { x }
  
Exprs :
    e = Expr SEMI l = Exprs { e :: l }
  | e = Expr { [e] }
  |    { [] }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetExpr :
    LET x=Name EQ e=Expr n=AndDecl IN ex=Expr { LetExp (Let ((x, e)::n), ex) }
  | LET x=Name e=EQFun n=AndDecl IN ex=Expr { LetExp(Let ((x, e)::n), ex) }
  | LET REC x=Name e=EQFun n=AndDecl IN ex=Expr { LetExp (Letrec ((x, e)::n), ex) }
  | LET REC x=Name EQ e=FunExpr n=AndDecl IN ex=Expr { LetExp (Letrec ((x, e)::n), ex) }

FunExpr :
    FUN e=ARFun { e }
  | DFUN e=DARFun { e }

MatchExpr :
    MATCH e1 = Expr WITH LBRACKET RBRACKET RARROW e2 = Expr PIPE x1 = Name CONS x2=Name RARROW e3 = Expr 
      { MatchExp (e1, e2, x1, x2, e3) }
EQFun :
    x=Name e=EQFun { FunExp (x, e) }
  | x=Name EQ e=Expr { FunExp (x, e) }

ARFun :
    x=Name e=ARFun { FunExp (x, e) }
  | x=Name RARROW e=Expr { FunExp (x, e) }

DARFun :
    x=Name e=DARFun { DFunExp (x, e) }
  | x=Name RARROW e=Expr { DFunExp (x, e) }
    