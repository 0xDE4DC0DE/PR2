type ide = string;;

type exp = Eint of int 
	| Ebool of bool 
	| Den of ide 
	| Prod of exp * exp 
	| Sum of exp * exp 
	| Diff of exp * exp 
	| Eq of exp * exp 
	| Minus of exp 
	| IsZero of exp 
	| Or of exp * exp 
	| And of exp * exp 
	| Not of exp 
	| Ifthenelse of exp * exp * exp 
	| Let of ide * exp * exp 
	| Fun of ide * exp 
	| FunCall of exp * exp 
	| Letrec of ide * exp * exp
	(* Dizionario *)
	| Dict of (ide * exp) list
	| Insert of ide * exp * exp
	| Delete of exp * ide
	| HasKey of ide * exp
	| Iterate of exp * exp
	| Fold of exp * exp
	| FunAcc of ide * ide * exp
	| FunAccCall of exp * exp * exp
	| Filter of (ide list) * exp;;

(*ambiente polimorfo*)
type 't env = ide -> 't;;
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;

(*tipi esprimibili*)
type evT = 
	| Int of int 
	| Bool of bool 
	| Unbound 
	| FunVal of evFun 
	| RecFunVal of ide * evFun
	| DictVal of (ide * evT) list
	| FunAccVal of evFunAcc
and evFun = ide * exp * evT env
and evFunAcc = ide * ide * exp * evT env;;

(*rts*)
(*type checking*)
let rec typecheck (s : string) (v : evT) : bool = match s with
	| "int" -> (match v with
		| Int(_) -> true
		| _ -> false) 
	| "bool" -> (match v with
		| Bool(_) -> true
		| _ -> false)
	| "dict" -> (match v with
		| DictVal(d) -> (match d with
				| [] -> true
				| (key, value)::ds -> if (typecheck "int" value) then typechecklist "int" ds
								   else typechecklist "bool" ds
				|_ -> failwith("typechecker error"))
		|_ -> false)
	| _ -> failwith("not a valid type")
	and typechecklist (s: string) (l:(ide * evT) list) : bool = (match l with
		| [] -> true
		| (id,x)::ls -> (typecheck s x) && (typechecklist s ls));;	

(*funzioni primitive*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		| (Int(n),Int(u)) -> Int(n*u)
		| (_,_) -> failwith("run-time error"))
	else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		| (Int(n),Int(u)) -> Int(n+u)
		| (_,_) -> failwith("run-time error"))
	else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		| (Int(n),Int(u)) -> Int(n-u)
		| (_,_) -> failwith("run-time error"))
	else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		| (Int(n),Int(u)) -> Bool(n=u)
		| (_,_) -> failwith("run-time error"))
	else failwith("Type error");;

let minus x = if (typecheck "int" x) 
	then (match x with
		   | Int(n) -> Int(-n)
		   | _ -> failwith("run-time error"))
	else failwith("Type error");;

let iszero x = if (typecheck "int" x)
	then (match x with
		| Int(n) -> Bool(n=0)
		| _ -> failwith("run-time error"))
	else failwith("Type error");;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		| (Bool(b),Bool(e)) -> (Bool(b||e))
		| (_,_) -> failwith("run-time error"))
	else failwith("Type error");;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		| (Bool(b),Bool(e)) -> Bool(b&&e)
		| (_,_) -> failwith("run-time error"))
	else failwith("Type error");;

let non x = if (typecheck "bool" x)
	then (match x with
		| Bool(true) -> Bool(false)
		| Bool(false) -> Bool(true)
		| _ -> failwith("run-time error"))
	else failwith("Type error");;


(*interprete*)

let rec eval (e : exp) (r : evT env) : evT = match e with
	| Eint n -> Int n
	| Ebool b -> Bool b
	| IsZero a -> iszero (eval a r)
	| Den i -> applyenv r i
	| Eq(a, b) -> eq (eval a r) (eval b r)
	| Prod(a, b) -> prod (eval a r) (eval b r)
	| Sum(a, b) -> sum (eval a r) (eval b r)
	| Diff(a, b) -> diff (eval a r) (eval b r)
	| Minus a -> minus (eval a r)
	| And(a, b) -> et (eval a r) (eval b r)
	| Or(a, b) -> vel (eval a r) (eval b r)
	| Not a -> non (eval a r)
	| Ifthenelse(a, b, c) -> 
		let g = (eval a r) in
			if (typecheck "bool" g) 
				then (if g = Bool(true) then (eval b r) else (eval c r))
				else failwith ("nonboolean guard")
	| Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r))
	| Fun(i, a) -> FunVal(i, a, r)
	| FunCall(f, eArg) -> 
		let fClosure = (eval f r) in
			(match fClosure with
				| FunVal(arg, fBody, fDecEnv) -> 
					eval fBody (bind fDecEnv arg (eval eArg r))
				| RecFunVal(g, (arg, fBody, fDecEnv)) -> 
					let aVal = (eval eArg r) in
						let rEnv = (bind fDecEnv g fClosure) in
							let aEnv = (bind rEnv arg aVal) in
								eval fBody aEnv
				| _ -> failwith("non functional value"))
	| Letrec(f, funDef, letBody) ->
        		(match funDef with
					| Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
                         			                eval letBody r1
					| _ -> failwith("non functional def"))
	
	(* DIZIONARIO *)
	
	| FunAcc (acc,i,a) -> FunAccVal(acc,i,a,r) 
	| FunAccCall(f,eAcc,eArg) -> 
		let fclosure = (eval f r) in 
			(match fclosure with
	   			FunAccVal(acc,arg,fBody,fDecEnv) -> let env = bind fDecEnv arg (eval eArg r)
					in eval fBody (bind env acc (eval eAcc r)))
	| Dict(list) -> let rec eval_list (l:(ide * exp) list) (r: evT env) : (ide * evT) list = (match l with 
				| [] -> []
				| (k, v)::l -> (k, eval v r)::(eval_list l r) )
				in DictVal (eval_list list r)
	| Insert(key, value, dict) -> (match eval dict r with 
		| DictVal d ->
			let rec insert (key: ide) (value: evT) (dict: (ide * evT) list) : (ide * evT) list = (match dict with
				| [] -> (key, value)::[]
				| (k, v)::l -> if (key = k) then (k, v)::l else (k, v)::(insert key value l)) 
			in DictVal(insert key (eval value r) d)
		| _ -> failwith ("dict is not a dictionary"))

	| Delete(dict, key) -> (match eval dict r with
		| DictVal d -> let rec delete (key: ide) (dict: (ide * evT) list) : (ide * evT) list = 
			match dict with
				| [] -> []
				| (k, v)::l -> if (key=k) then l else (k, v)::(delete key l)
			in DictVal(delete key d)
		| _ -> failwith("dict is not a dictionary"))
	| HasKey(key, dict) -> (match eval dict r with
		| DictVal d -> let rec haskey (dict: (ide * evT) list) (key: ide) : bool =
			match dict with
				| [] -> false
				| (k, v) :: l -> if (k=key) then true else haskey l key 
			in Bool(haskey d key)
		| _ -> failwith("dict is not a dictionary"))
	| Iterate(func, dict) -> (match eval dict r with
        | DictVal d -> if (typecheck "dict" (DictVal(d))) then (DictVal(iterate func d r)) 
                else failwith("dynamic typechecker error")
		| _ -> failwith("dict is not a dictionary"))
	| Fold(func, dict) -> (match eval dict r with
		| DictVal d -> if (typecheck "dict" (DictVal(d))) then (match func with
							| FunAcc(_,_,Sum(_)) -> fold func d (Int(0)) r
							| FunAcc(_,_,Prod(_)) -> fold func d (Int(1)) r
							| FunAcc(_,_,Diff(_)) -> fold func d (Int(0)) r
							| FunAcc(_,_,And(_)) -> fold func d (Bool(true)) r
							| FunAcc(_,_,Or(_)) -> fold func d (Bool(false)) r       
                            | _ -> failwith("unable to apply fold with that function"))
                        else failwith("dynamic typechecker error")
		| _ -> failwith("dict is not a dictionary"))
	| Filter(keylist, dict) -> (match eval dict r with 
		| DictVal(d) -> let rec filter (l: ide list) (dict: (ide * evT) list) (r: evT env) : (ide * evT) list = 
			match dict with
				| [] -> []
				| (k, v)::ls -> if (List.mem k l) then (k, v)::(filter l ls r) else (filter l ls r) 
			in DictVal(filter keylist d r)
        | _ -> failwith("dict is not  dictionary"))
    and iterate (func:exp) (l:(ide*evT) list) (r:evT env) : (ide*evT) list = (match l with
        | [] -> []
        | (id,x)::xs -> (match x with
                         Int(u) ->let value = eval (FunCall(func,Eint(u))) r
                                  in (id,value) :: iterate func xs r
                         |Bool(w) ->let value = eval (FunCall(func,Ebool(w))) r
                                   in (id,value) :: iterate func xs r
                         |_ -> failwith("unable to apply iterate with that valtype in dictionary")))
	and fold (func:exp) (d:(ide*evT) list) (e:evT) (r:evT env)  : evT  = (match d with
		| [] -> e
		| (id,x)::xs -> (match (e,x) with
							| (Int(u), Int(w)) -> fold func xs (eval (FunAccCall(func,(Eint(u)),Eint(w))) r) r
							| (Bool(u), Bool(w)) -> fold func xs (eval (FunAccCall(func,(Ebool(u)),Ebool(w))) r) r
							| _ -> failwith ("an error as occurred in fold")));;



(* =============================  TESTS  ============================= *)

(* no let *)
let env0 = emptyenv Unbound;;

let e1 = FunCall(Fun("y", Sum(Den "y", Eint 1)), Eint 3);;

eval e1 env0;;

(* let *)
let e2 = FunCall(Let("x", Eint 2, Fun("y", Sum(Den "y", Den "x"))), Eint 3);;

eval e2 env0;;

(* ============================ DIZIONARIO ============================ *)

(* Empty env *)
let env0 = emptyenv Unbound;;

(* Creo dizionario alfabeto *)
let alfabeto = Dict([("a", Eint(10)); ("b", Eint(20)); ("c", Eint(30)); ("d", Eint(40))]);;

(* Inserisco un elemento nel dizionario alfabeto *)
eval (Insert("e", Eint(50), alfabeto)) env0;;
(* Provo a inserire un elemento che gia esiste nel dizionario *)
eval (Insert("e", Eint(50), alfabeto)) env0;;

(* Elimino un elemento dal dizionario alfabeto *)
eval (Delete(alfabeto, "e")) env0;;
(* Provo ad eliminare un elemento che non esiste dal dizionario *)
eval (Delete(alfabeto, "z")) env0;;

(* Cerco un elemento nel dizionario alfabeto *)
eval (HasKey("a", alfabeto)) env0;;
(* Provo a cercare un elemento che non esiste nel dizionario *)
eval (HasKey("z", alfabeto)) env0;;

(* Dichiaro una funzione f che raddoppia i valori delle chiavi del dizionario *)
let f = Fun("x", Prod(Den "x", Eint(2)));;
(* Applico la funzione f al dizionario alfabeto *)
eval (Iterate(f, alfabeto)) env0;;

(* Dichiaro una funzione func che somma i valori delle chiavi del dizionario *)
let func = FunAcc("acc", "x", Sum(Den "acc", Den "x"));;
(* Applico la funzione func al dizionario alfabeto *)
eval (Fold(func, alfabeto)) env0;;
(* Dichiaro una funzione func2 che equaglia tutti i valori delle chiavi del dizionario *)
let func2 = FunAcc("acc", "x", Eq(Den "acc", Den "x"));;
(* Provo a appliare func2 al dizionario alfabeto *)
eval (Fold(func2, alfabeto)) env0;;

(* Creo una lista di chiavi da tenere nell'alfabeto *)
let l = ["a"; "b"];;
(* Filtro il dizionario con la lista l, restano solo "a" e "b" *)
eval (Filter(l, alfabeto)) env0;;
