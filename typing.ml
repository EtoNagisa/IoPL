open Syntax

exception Error of string
let err s = raise (Error s)

type tyenv = ty Environment.t

type tyvarenv = string Environment.t
type subst = (tyvar * ty) list

let rec freevar_ty = function
	TyInt -> MySet.empty
|	TyBool -> MySet.empty
|	TyVar x -> MySet.singleton x
|	TyFun (ty1, ty2) -> MySet.union (freevar_ty ty1) (freevar_ty ty2)


let nth_tyvar n =
	if n<26 then Char.escaped (char_of_int (int_of_char ('a') + n))
	else Char.escaped (char_of_int (int_of_char ('a') + n mod 26)) ^ string_of_int (n/26)

let attr_tyvar ty =
	let n = ref 0 in
	let rec attr_tyvar tyvarenv= function
		TyInt -> tyvarenv
	|	TyBool -> tyvarenv
	|	TyVar a -> 
			(try ignore(Environment.lookup (string_of_int a) tyvarenv);tyvarenv
			with  
				Environment.Not_bound ->
					let v = !n in
					(n := v + 1;
					Environment.extend (string_of_int a) (nth_tyvar v) tyvarenv))
	|	TyFun (ty1, ty2) ->
			let newenv = attr_tyvar tyvarenv ty1 in
			attr_tyvar newenv ty2
	in attr_tyvar Environment.empty ty

let pp_ty ty = 
	let tyvarenv = attr_tyvar ty in
	let rec pp_ty = function 
		TyInt -> print_string "int"
	| 	TyBool -> print_string "bool"
	|	TyVar tyvar -> print_string ("'" ^ Environment.lookup (string_of_int tyvar) tyvarenv)
	|	TyFun (ty1, ty2) -> 
			(match ty1 with
				TyFun _ -> 
					print_string "(";pp_ty ty1;print_string ")"
			|	_ -> pp_ty ty1);
			print_string " -> ";
			pp_ty ty2
	in pp_ty ty

let rec print_subst = function
	[] -> ()
|	(tyvar, ty) :: rest ->
		print_int tyvar;
		print_string " = ";
		pp_ty ty;
		print_subst rest

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

let rec subst_eqs st = function
	[] -> []
|	(ty1, ty2)::rest ->
		(subst_type st ty1, subst_type st ty2) :: subst_eqs st rest

let rec eqs_of_subst = function
	[] -> []
|	(tyv,ty) :: rest -> (TyVar tyv, ty) :: eqs_of_subst rest


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
		|	TyVar a, TyInt -> (a, TyInt) :: unify (subst_eqs [(a, TyInt)] rest)
		|	TyVar a, TyBool -> (a, TyBool) :: unify (subst_eqs [(a, TyBool)] rest)
		|	TyInt, TyVar a -> (a, TyInt) :: unify (subst_eqs [(a, TyInt)] rest)
		|	TyBool, TyVar a -> (a, TyBool) :: unify (subst_eqs [(a, TyBool)] rest)
		|	TyVar a, TyVar b ->
				if a=b then unify rest
				else (a, TyVar b) :: unify (subst_eqs [(a, TyVar b)] rest)
		|	TyVar a, TyFun _ ->
				if contains a ty2 then err ("type error in tyfun")
				else (a, ty2) :: unify (subst_eqs [(a, ty2)] rest)
		|	TyFun _, TyVar a ->
				if contains a ty1 then err ("type error in tyfun")
				else (a, ty1) :: unify (subst_eqs [(a, ty1)] rest)
		|	TyFun(a1,a2), TyFun(b1,b2) ->unify ((a1,b1)::(a2,b2)::rest)
		|	_ -> err ("type error")


let fresh_tyvar = 
	let counter = ref 0 in
	let body () =
		let v = !counter in
		counter := v + 1; v
	in  body
let ty_prim op ty1 ty2 = match op with
	Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
|	Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
|	Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
|	And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
|	Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

let rec ty_exp tyenv = function
	Var x ->
		(try ([],Environment.lookup x tyenv) with
		Environment.Not_bound -> err ("variable not bound: " ^ x))
| 	ILit _ -> ([], TyInt)
| 	BLit _ -> ([], TyBool)
| 	BinOp (op, exp1, exp2) ->
		let (s1, ty1) = ty_exp tyenv exp1 in
		let (s2, ty2) = ty_exp tyenv exp2 in
		let (eqs3, ty) = ty_prim op ty1 ty2 in
		let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
		let s3 = unify eqs in
		(s3, subst_type s3 ty)
| 	IfExp (exp1, exp2, exp3) ->
		let (s1, ty1) = ty_exp tyenv exp1 in
		let (s2, ty2) = ty_exp tyenv exp2 in
		let (s3, ty3) = ty_exp tyenv exp3 in
		let eqs1 = eqs_of_subst s1 in
		let eqs2 = eqs_of_subst s2 in
		let eqs3 = eqs_of_subst s3 in
		let eqs = (ty1, TyBool) :: (ty2, ty3) :: eqs1 @ eqs2 @ eqs3 in
		let s = unify eqs in (s,subst_type s ty3)
| 	LetExp (Let [(id,exp1)], exp2) ->
		let (s1, ty1) = ty_exp tyenv exp1 in
		let (s2, ty2) = ty_exp (Environment.extend id ty1 tyenv) exp2 in
		let eqs1 = eqs_of_subst s1 in
		let eqs2 = eqs_of_subst s2 in
		let eqs = eqs1 @ eqs2 in
		let s = unify eqs in (s, subst_type s ty2)
|	LetExp (Letrec [(id,exp1)], exp2) ->
		let tyv = TyVar (fresh_tyvar ()) in
		let (s1, ty1) = ty_exp (Environment.extend id tyv tyenv) exp1 in
		let (s2, ty2) = ty_exp (Environment.extend id ty1 tyenv) exp2 in
		let eqs1 = eqs_of_subst s1 in
		let eqs2 = eqs_of_subst s2 in
		let eqs =  (tyv, ty1) :: eqs1 @ eqs2 in
		let s = unify eqs in (s, subst_type s ty2)
|	FunExp (id, exp) ->
		let domty = TyVar (fresh_tyvar ()) in
		let (s, ranty) =
			ty_exp (Environment.extend id domty tyenv) exp in
			(s, TyFun (subst_type s domty, ranty))
|	AppExp (exp1, exp2) ->
		let (s1, ty1) = ty_exp tyenv exp1 in
		let (s2, ty2) = ty_exp tyenv exp2 in
		let eqs1 = eqs_of_subst s1 in
		let eqs2 = eqs_of_subst s2 in
		(match ty1 with
			TyFun (tyarg1, tyarg2) -> 
				let eqs = (tyarg1, ty2) :: eqs1 @ eqs2 in
				let s =unify eqs in (s,subst_type s tyarg2)
		|	TyVar a ->
				let ranty = TyVar (fresh_tyvar ()) in
				let eqs = (TyVar a, TyFun(ty2, ranty)) :: eqs1 @ eqs2 in
				let s = unify eqs in (s,subst_type s ranty)
		|	_ -> err ("non-function exp is applied"))
		
| _ -> err ("Not Implemented!")

let ty_decl tyenv = function
	Exp e -> [ty_exp tyenv e]
| 	_ -> err ("Not Implemented!")
