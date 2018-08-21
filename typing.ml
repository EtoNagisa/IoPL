open Syntax
open MySet

exception Error of string
let err s = raise (Error s)

type tysc = TyScheme of tyvar list * ty
type tyenv = tysc Environment.t
type tyset = tyvar MySet.t

type tyvarenv = string Environment.t
type subst = (tyvar * ty) list

let tysc_of_ty ty = TyScheme ([], ty)


let rec freevar_ty = function
	TyInt -> MySet.empty
|	TyBool -> MySet.empty
|	TyVar x -> MySet.singleton x
|	TyFun (ty1, ty2) -> MySet.union (freevar_ty ty1) (freevar_ty ty2)
|	TyList t -> freevar_ty t

let rec freevar_tysc tysc =
	let (l, ty1) = tysc in
	diff (freevar_ty ty1) (from_list l)

let freevar_tyenv tyenv = 
	let f (x:tysc) tyenv =
		match x with TyScheme (_, ty) ->
		match ty with
			TyVar a -> insert a tyenv
		|	TyFun (ty1, ty2) -> union (union (freevar_ty ty1) (freevar_ty ty2)) tyenv
		|	_ -> tyenv
	in
	Environment.fold_right f tyenv MySet.empty

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
				|	TyList t1 -> TyList (substi (var,ty) t1)
			in subst_type rest (substi (var,ty) t)

let rec subst_eqs st = function
	[] -> []
|	(ty1, ty2)::rest ->
		(subst_type st ty1, subst_type st ty2) :: subst_eqs st rest

let rec eqs_of_subst = function
	[] -> []
|	(tyv,ty) :: rest -> (TyVar tyv, ty) :: eqs_of_subst rest

let closure ty tyenv subst =
	let fv_tyenv2 = freevar_tyenv tyenv in
	let fv_tyenv =
		MySet.bigunion
			(MySet.map
				(fun id -> freevar_ty (subst_type subst (TyVar id)))
				fv_tyenv2) in
	let ids = MySet.diff (freevar_ty ty) fv_tyenv in
	TyScheme (MySet.to_list ids, ty)

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
	|	TyList t ->
			attr_tyvar tyvarenv t 
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
	|	TyList t -> 
			pp_ty t;
			print_string " list"
	in pp_ty ty


let pp_ty_zako = function

	TyVar tyvar -> print_string ("'" ^ (string_of_int tyvar))
|	t -> pp_ty t

let rec print_subst = function
	[] -> ()
|	(tyvar, ty) :: rest ->
		print_int tyvar;
		print_string " = ";
		pp_ty_zako ty;
		print_newline();
		print_subst rest

let rec print_sc =function
	[] -> ()
|	id::rest  ->
		print_int id;
		print_sc rest

let rec unify tl= 
	match tl with
	[] -> []
|	(ty1, ty2) :: rest ->
		match ty1,ty2 with
			TyInt, TyInt -> unify rest
		|	TyBool, TyBool -> unify rest
		|	TyVar a, TyInt | TyInt, TyVar a -> (a, TyInt) :: unify (subst_eqs [(a, TyInt)] rest)
		|	TyVar a, TyBool | TyBool, TyVar a-> (a, TyBool) :: unify (subst_eqs [(a, TyBool)] rest)
		|	TyVar a, TyVar b ->
				if a=b then unify rest
				else (a, TyVar b) :: unify (subst_eqs [(a, TyVar b)] rest)
		|	TyVar a, TyFun (t1,t2) | TyFun (t1,t2), TyVar a->
				if member a (freevar_ty (TyFun (t1,t2))) then err ("type error in tyfun")
				else (a, (TyFun (t1,t2))) :: unify (subst_eqs [(a, (TyFun (t1,t2)))] rest)
		|	TyFun(a1,a2), TyFun(b1,b2) ->unify ((a1,b1)::(a2,b2)::rest)
		|	TyVar a, TyList t | TyList t, TyVar a -> 
				if member a (freevar_ty (TyList t)) then err("type error in tylist")
				else (a,TyList t) :: unify (subst_eqs [(a,TyList t)] rest)
		|	TyList a, TyList b ->
				unify ((a,b)::rest)
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
|	Cons -> ([(TyList ty1,ty2)], ty2)

let rec ty_exp tyenv = function
	Var x ->
		(try 
			let TyScheme (vars, ty) = Environment.lookup x tyenv in
			let s = List.map (fun id -> (id, TyVar (fresh_tyvar ()))) vars in
			([], subst_type s ty)
		with Environment.Not_bound -> err ("variable not bound: " ^ x))
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
| 	LetExp (l, exp) ->
		let (newenv, (s1, _)) = ty_decls tyenv [l] in
		let (s2, ty2) = ty_exp newenv exp in
		let eqs1 = eqs_of_subst s1 and eqs2 = eqs_of_subst s2 in
		let eqs = eqs1 @ eqs2 in
		let s = unify eqs in (s, subst_type s ty2)
|	FunExp (id, exp) ->
		let domty = TyVar (fresh_tyvar ()) in
		let (s, ranty) =
			ty_exp (Environment.extend id (TyScheme ([], domty)) tyenv) exp in
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
|	MatchExp (exp1,exp2,id1,id2,exp3) ->
		let (s1,ty1) = ty_exp tyenv exp1 and (s2,ty2) = ty_exp tyenv exp2 in
		let tyv = fresh_tyvar() in
		let (s3,ty3) = ty_exp (Environment.extend id1 (TyScheme([],TyVar tyv))
								(Environment.extend id2 (TyScheme([],TyList (TyVar tyv))) tyenv)) exp3 in
		let eqs = (ty1, TyList (TyVar tyv)) :: (ty2, ty3) :: (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3)
		in let  s = unify eqs in (s,subst_type s ty2)
|	ListExp l ->
		(match l with
			[] -> ([], TyList (TyVar (fresh_tyvar())))
		| x::rest ->
			let rec make_tylist = function
				[] -> [],[]
			|	(e::rest) ->
					let (s,ty) = ty_exp tyenv e in
					let (rs,rty) = make_tylist rest in
					(s::rs,ty::rty) 
			in let (s0,tyl) = make_tylist l in
			let rec ty_list = function
				[],[] -> []
			|	([s],[t]) -> eqs_of_subst s 
			|	(s::srest,ty1::ty2::trest) ->
					(ty1,ty2) :: eqs_of_subst s  @ ty_list (srest,ty2::trest) 	

			in let eqs = ty_list (s0,tyl) in
			let (_, ty1) = ty_exp tyenv x in 
			let s=unify eqs in (s,TyList (subst_type s ty1)))
		
| _ -> err ("Not Implemented!")



and ty_decl tyenv = function
	[] -> (tyenv, ([], []))
|	(id, e) :: rest ->
		let (s,ty) = ty_exp tyenv e in
		let (renv, (rs,rty)) = ty_decl tyenv rest in
		(Environment.extend id (closure ty tyenv s) renv, (s @ rs, ty :: rty))

	
	
and ty_rec_decl tyenv l = 
	let rec make_list tyenv = function
		[] -> ([], [], tyenv)  
	|	(id, e) :: rest -> 
			let tyv = TyVar (fresh_tyvar ()) in
			let (ids, tyl, retenv) = make_list tyenv rest in
			(id :: ids, tyv :: tyl , Environment.extend id (TyScheme ([], tyv)) retenv)
	in let (ids, tyvarl, newenv) = make_list tyenv l in
	
	let rec ty_all tyvarl l = 
		match tyvarl,l with
			[], [] -> ([], [])
		|	v :: vrest, (id, e) :: rest ->
				let (s,ty) = ty_exp newenv e in
				let (reqs,rty) = ty_all vrest rest in
				((v, ty) :: (eqs_of_subst s) @ reqs , ty :: rty)
	in let (eqs,ty) = ty_all tyvarl l in
	let s = unify eqs in
	let rec make_tyenv tyenv ids l =
		match ids,l with
		[], [] -> (tyenv, [])
	|	id :: idrest, ty :: rest ->
			let (retenv, t) = make_tyenv tyenv idrest rest in
			let ty_s = subst_type s ty in
			((Environment.extend id (closure ty_s tyenv s) retenv), ty_s :: t)
	in let (retenv, retty) = make_tyenv tyenv ids ty in
	(retenv, (s,retty))


	
	
and ty_decls tyenv = function
	[] -> (tyenv,([], []))
|	(Let l) :: rest ->
		let (newenv, (s ,str)) = ty_decl tyenv l in
		let (retenv, (rets,retstr)) = ty_decls newenv rest in
		(retenv, (s@rets, str@retstr))
|	(Letrec l) :: rest ->
		let (newenv, (s, str)) = ty_rec_decl tyenv l in
		let (retenv, (rets,retstr)) = ty_decls newenv rest in
		(retenv, (s@rets, str@retstr)) 
		
let ty tyenv = function
	Exp e -> 
		let (s,ty) = ty_exp tyenv e in
		(tyenv, [ty]);
| 	Decl e ->
		let (tyenv, (s,ty)) = ty_decls tyenv e in
		(tyenv, ty)