type funName = Fid of string;;
type funArg = Ide of string;;

type exp = CstInt of int
    | CstTrue
    | CstFalse
    | Sum of exp * exp
    | Times of exp * exp
    | Sub of exp * exp
    | Eq of exp * exp
    | Iszero of exp
    | Or of exp * exp
    | And of exp * exp
    | Not of exp
    | Ifthenelse of exp * exp * exp
    | Val of string
    | Apply of funName * exp;;

type evT = Int of int | Bool of bool;;

type funBody = Body of exp;;
type funDef = Fun of funName * funArg * funBody;;
type funDecl = List of funDef;;

let rec getFunDef (n, l) = match (n, l) with
    | (Fid(s1), Fun(Fid(s2), Ide(af), Body(b))::ls) -> if s1=s2 then Fun(Fid(s2), Ide(af), Body(b)) else getFunDef(n, ls)
    | (Fid _, []) -> failwith("run-time error");;
let getFunArg f = match f with 
    | Fun(Fid(nf), Ide(na), Body(b)) -> Ide(na);;
let getFunBody f = match f with 
    | Fun(Fid(nf), Ide(na), Body(b)) -> Body(b);;


type exp_ext = CstIntExt of int
    | SumExt of exp_ext * exp_ext
    | TimesExt of exp_ext * exp_ext
    | Minus of exp_ext * exp_ext;;

let rec translate (e: exp_ext) : exp = match e with 
    | CstIntExt i -> CstInt i
    | SumExt (e1, e2) -> Sum (translate(e1), translate(e2))
    | TimesExt (e1, e2) -> Times (translate(e1), translate(e2))
    | Minus (e1, e2) -> Sum (translate(e1), Times(CstInt(-1), translate(e2)));;

let typecheck (x, y) = match x with 
    | "int" -> (match y with | Int(u) -> true | _ -> false)
    | "bool" -> (match y with | Bool(u)-> true | _ -> false)
    | _ -> failwith("not a valid type");;

let is_zero x = match (typecheck("int", x), x) with 
    | (true, Int (y)) -> Bool(y=0)
    | (_, _) -> failwith ("run-time error");;
let int_eq(x, y) = match (typecheck("int", x), typecheck("int", y), x, y) with 
    | (true, true, Int(v), Int(w)) -> Bool (v=w)
    | (_,_,_,_) -> failwith ("run-time error");;
let int_plus(x, y) = match (typecheck("int", x), typecheck("int", y), x, y) with 
    | (true, true, Int(v), Int(w)) -> Int (v+w)
    | (_,_,_,_) -> failwith ("run-time error");;
let int_times(x, y) = match (typecheck("int", x), typecheck("int", y), x, y) with 
    | (true, true, Int(v), Int(w)) -> Int (v*w)
    | (_,_,_,_) -> failwith ("run-time error");;
let int_sub(x, y) = match (typecheck("int", x), typecheck("int", y), x, y) with 
    | (true, true, Int(v), Int(w)) -> Int (v-w)
    | (_,_,_,_) -> failwith ("run-time error");;
let bool_and(x, y) = match (typecheck("bool", x), typecheck("bool", y), x, y) with 
    | (true, true, Bool(v), Bool(w)) -> Bool (v&&w)
    | (_,_,_,_) -> failwith ("run-time error");;
let bool_or(x, y) = match (typecheck("bool", x), typecheck("bool", y), x, y) with 
    | (true, true, Bool(v), Bool(w)) -> Bool (v||w)
    | (_,_,_,_) -> failwith ("run-time error");;
let bool_not(x) = match (typecheck("bool", x), x) with 
    | (true, Bool(v)) -> Bool(not(v))
    | (_,_) -> failwith ("run-time error");;

let rec subst (e1, arg, e2) = match e2 with
    | CstInt(n) -> CstInt(n)
    | CstTrue -> CstTrue
    | CstFalse -> CstFalse
    | Iszero(e) -> Iszero(subst(e1, arg, e))
    | Eq(ea, eb) -> Eq(subst(e1, arg, ea), subst(e1, arg, eb))
    | Times(ea, eb) -> Times(subst(e1, arg, ea), subst(e1, arg, eb))
    | Sum(ea, eb) -> Sum(subst(e1, arg, ea), subst(e1, arg, eb))
    | Sub(ea, eb) -> Sub(subst(e1, arg, ea), subst(e1, arg, eb))
    | And(ea, eb) -> And(subst(e1, arg, ea), subst(e1, arg, eb))
    | Or(ea, eb) -> Or(subst(e1, arg, ea), subst(e1, arg, eb))
    | Not(e) -> Not(subst(e1, arg, e))
    | Ifthenelse(eg, et, ef) -> Ifthenelse(subst(e1, arg, eg), subst(e1, arg, et), subst(e1, arg, ef))
    | Val(s) -> (match arg with 
                    | Ide(s) -> e1)
                    (*| _ -> Val(s))*)
    | Apply(f, e) -> Apply(f, subst (e1, arg, e));;
    (*| _ -> failwith("run-time error");;*)

let rec eval (e, dcl) = match e with
    | CstInt(n) -> Int(n)
    | CstTrue -> Bool(true)
    | CstFalse -> Bool(false)
    | Iszero(e1) -> is_zero(eval(e1, dcl))
    | Eq(e1, e2) -> int_eq(eval(e1, dcl), eval(e2, dcl))
    | Times(e1, e2) -> int_times(eval(e1, dcl), eval(e2, dcl))
    | Sum(e1, e2) -> int_plus(eval(e1, dcl), eval(e2, dcl))
    | Sub(e1, e2) -> int_sub(eval(e1, dcl), eval(e2, dcl))
    | And(e1, e2) -> bool_and(eval(e1, dcl), eval(e2, dcl))
    | Or(e1, e2) -> bool_or(eval(e1, dcl), eval(e2, dcl))
    | Not(e1) -> bool_not(eval(e1, dcl))
    | Apply(f, a) -> let t = getFunDef (f, dcl) in let es = subst(a, getFunArg(t), getFunBodyExp(t)) in eval (es, dcl)
    | Val("x") -> failwith ("run-time error: unbound name")
    | Ifthenelse(e1, e2, e3) -> 
        let g = eval(e1, dcl) in (match (typecheck("bool", g), g) with
            | (true, Bool(true)) -> eval(e2, dcl)
            | (true, Bool(false)) -> eval(e3, dcl)
            | (_,_) -> failwith("nonboolean guard"))
    | _ -> failwith("run-time error");;
