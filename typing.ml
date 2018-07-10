open Syntax
exception Error of string
let err s = raise (Error s)

type tyenv = ty Environment.t



type subst = (tyvar * ty) list

let rec subst_type st t =
	match st with
		[] -> t
	|	(var,ty) :: rest ->
			let rec substi (var,ty) t =
				match t with
					TyInt -> TyInt 
				|	TyBool -> TyBool
				|	TyVar v ->
						if v = var then ty
						else TyVar v
				|	TyFun (ty1, ty2) -> TyFun (substi (var,ty) ty1,substi (var,ty) ty2)
			in subst_type rest (substi (var,ty) t)

let rec subst_one st = function
	[] -> []
|	(ty1, ty2)::rest ->
		(subst_type [st] ty1, subst_type [st] ty2) :: subst_one st rest

let rec contains tyv = function
	TyInt -> false
|	TyBool -> false
|	TyVar a ->
		tyv = a
|	TyFun (ty1, ty2) -> 
		(contains tyv ty1) || (contains tyv ty2)

let rec unify = function 
	[] -> []
|	(ty1, ty2) :: rest ->
		match ty1,ty2 with
			TyInt, TyInt -> unify rest
		|	TyBool, TyBool -> unify rest
		|	TyVar a, TyInt -> unify (subst_one (a, TyInt) rest)
		|	TyVar a, TyBool -> unify (subst_one (a, TyBool) rest)
		|	TyInt, TyVar a -> unify (subst_one (a, TyInt) rest)
		|	TyBool, TyVar a -> unify (subst_one (a, TyBool) rest)
		|	TyVar a, TyVar b ->
				if a=b then unify rest
				else unify (subst_one (a, TyVar b) rest)
		|	TyVar a, TyFun _ ->
				if contains a ty2 then err ("type error in tyfun")
				else (a, ty2) :: unify (subst_one (a, ty2) rest)
		|	TyFun _, TyVar a ->
				if contains a ty1 then err ("type error in tyfun")
				else (a, ty1) :: unify (subst_one (a, ty1) rest)
		|	TyFun(a1,a2), TyFun(b1,b2) ->unify ((a1,b1)::(a2,b2)::rest)
		|	_ -> err ("type error")

let rec pp_ty = function
	TyInt -> print_string "int"
| 	TyBool -> print_string "bool"
|	TyVar tyvar -> print_string ("'" ^ string_of_int tyvar)
|	TyFun (ty1, ty2) -> 
		(match ty1 with
			TyFun _ -> 
				print_string "(";pp_ty ty1;print_string ")"
		|	_ -> pp_ty ty1);
		print_string " -> ";
		pp_ty ty2

let fresh_tyvar = 
	let counter = ref 0 in
	let body () =
		let v = !counter in
		counter := v + 1; v
	in  body
let ty_prim op ty1 ty2 = match op with
	Plus -> 
		(match ty1, ty2 with
			TyInt, TyInt -> TyInt
		| 	_ -> err ("Argument must be of integer: +"))
|	Mult ->
		(match ty1, ty2 with
			TyInt, TyInt -> TyInt
		|	_ -> err ("Argument must be of integer: *"))
|	Lt ->
		(match ty1, ty2 with
			TyInt, TyInt -> TyBool
		| 	_ -> err ("Argument must be of integer: <"))
|	And ->
		(match ty1, ty2 with
			TyBool, TyBool -> TyBool
		| 	_ -> err ("Argument must be of boolean: &&"))
|	Or ->
		(match ty1, ty2 with
			TyBool, TyBool -> TyBool
		| 	_ -> err ("Argument must be of boolean: ||"))


let rec ty_exp tyenv = function
	Var x ->
		(try Environment.lookup x tyenv with
		Environment.Not_bound -> err ("variable not bound: " ^ x))
| 	ILit _ -> TyInt
| 	BLit _ -> TyBool
| 	BinOp (op, exp1, exp2) ->
		let tyarg1 = ty_exp tyenv exp1 in
		let tyarg2 = ty_exp tyenv exp2 in
		ty_prim op tyarg1 tyarg2
| 	IfExp (exp1, exp2, exp3) ->
		let tyarg1 = ty_exp tyenv exp1 in
		let tyarg2 = ty_exp tyenv exp2 in
		let tyarg3 = ty_exp tyenv exp3 in
		(match tyarg1 with
			TyBool ->
				(match tyarg2,tyarg3 with
					TyInt, TyInt -> TyInt
				|	TyBool, TyBool -> TyBool
				|	_	-> err ("type mismatch between then and else"))
		|	_ -> err ("test exp must be boolean"))
| 	LetExp (Let [(id,exp1)], exp2) ->
		let tyarg1 = ty_exp tyenv exp1 in
		let newenv = Environment.extend id tyarg1 tyenv in
		ty_exp newenv exp2
| _ -> err ("Not Implemented!")

let ty_decl tyenv = function
	Exp e -> [ty_exp tyenv e]
| 	_ -> err ("Not Implemented!")
