(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr
  


  

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;

let e4 = Prim("==", CstI 1, CstI 1)
let e5 = Prim("max", CstI 2, CstI 1)
let e6 = Prim("min", CstI 3, CstI 5)

let e7 = If(Var "a", CstI 11, CstI 22)


(* Evaluation within an environment *)

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim(op, e1, e2) ->
        let first = eval e1 env
        let second = eval e2 env
        
        match op with
        | "-" -> first - second
        | "==" -> if first = second then 1 else 0
        | "max" -> if first < second then first else second
        | "min" -> if first < second then first else second
        | _ -> failwith "unknown primitive"
        
    | If(e1, e2, e3) ->
        if eval e1 env <> 0 then eval e2 env else eval e3 env
        
let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;

let e4v = eval e4 env
let e5v = eval e5 env
let e6v = eval e6 env
let e7v = eval e7 env


type aexpr = 
  | CstI of int
  | Var of string
  | Add of aexpr * aexpr
  | Mul of aexpr * aexpr
  | Sub of aexpr * aexpr
  
let e8 = Sub(Var "v", Add(Var "w", Var "z"))

let e9 = Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")))

let e10 = Add(Add(Add(Var "x", Var "y"), Var "z"), Var "v")

let e11 = Add(Var "x", CstI 0)

let e12 = Add(CstI 1, CstI 0)

let e13 = Mul(Add(CstI 1, CstI 0), Add(Var "x", CstI 0))

let rec fmt (e: aexpr) : string =
    match e with
    |CstI a -> string a
    |Var a -> a
    |Add(aexpr, aexpr1) -> $"({fmt aexpr} + {fmt aexpr1})"
    |Mul(aexpr, aexpr1) -> $"({fmt aexpr} * {fmt aexpr1})"
    |Sub(aexpr, aexpr1) -> $"({fmt aexpr} - {fmt aexpr1})"
    
let rec simplify (e: aexpr): aexpr =
    match e with
    |Add(a, b) ->
        match simplify a, simplify b with
        | CstI 0, x -> x
        | x, CstI 0 -> x
        | x,y -> Add(x,y)
    |Sub(a, b) ->
        match simplify a, simplify b with
        | x, CstI 0 -> x
        | x, y when x = y -> CstI 0
        | x,y -> Sub(x,y)
    |Mul(a, b) ->
        match simplify a, simplify b with
        | CstI 1, x -> x
        | x, CstI 1 -> x
        | CstI 0, _ -> CstI 0
        | _ , CstI 0 -> CstI 0
        | x,y -> Mul(x,y)
    | _ -> e
        
let rec diff e s =
    match e with
    |CstI _ -> CstI 0
    |Var s1 -> if s1 = s then CstI 1 else CstI 0
    |Add(aexpr, aexpr1) -> Add(diff aexpr s, diff aexpr1 s)
    |Sub(aexpr, aexpr1) -> Sub(diff aexpr s, diff aexpr1 s)
    |Add(aexpr, aexpr1) -> Add(Mul((diff aexpr s),aexpr1), Mul(diff aexpr s, (diff aexpr1 s)))