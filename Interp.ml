open Syntax

type typ   = TNum | TBool
type value = VNum of int | VBool of bool

type env = (var * value) list

type fdescr =
  Builtin |
  UDef of fname * fenv * env * var list * expr
and fenv = (fname * fdescr) list

exception UndefinedVariable of var

let rec lookup env x =
  match env with
  | [] -> raise (UndefinedVariable x)
  | (y, v) :: env ->
     if x = y then v
     else lookup env x

exception TypeError of typ * value
exception WrongArgNumber

let rec mkEnv xs vs env =
  match xs, vs with
  | [], [] -> env
  | x :: xs, v :: vs -> mkEnv xs vs ((x, v) :: env)
  | [], _ :: _ | _ :: _, [] -> raise WrongArgNumber

(* look at function name f, look at values,
   compute answer *)
let callBI f vs =
  match f, vs with
  | "add", [VNum m; VNum n] -> VNum (m + n)
  | "sub", [VNum m; VNum n] -> VNum (m - n)
  | "mul", [VNum m; VNum n] -> VNum (m * n)
  | "div", [VNum m; VNum n] -> VNum (m / n)
  | "mod", [VNum m; VNum n] -> VNum (m mod n)
  | "eq",  [VNum m; VNum n] -> VBool (m = n)
  | "lt",  [VNum m; VNum n] -> VBool (m < n)
  | "gt",  [VNum m; VNum n] -> VBool (m > n)
  | _, _ -> failwith "Some error"

(* eval: take an environment, a function envrionment
   and an expression, and
   produce a value*)
let rec eval env (fenv : fenv) e =
  match e with
  | Num  n -> VNum n
  | Bool b -> VBool b
  | Var x  -> lookup env x
  | App (f, es) ->
     let vs = List.map (eval env fenv) es
     in call f (lookup fenv f) vs
  | Let (x, e, e') ->
     let v = eval env fenv e
     in eval ((x, v) :: env) fenv e'
  | If (e, et, ef) ->
     match eval env fenv e with
     | VBool true  -> eval env fenv et
     | VBool false -> eval env fenv ef
     | v -> raise (TypeError (TBool, v))

and call f fd vs =
  match fd with
  | Builtin -> callBI f vs
  | UDef (f, fenv, env, xs, e) ->
     eval (mkEnv xs vs env) ((f, fd) :: fenv) e

          (*

            (def (gcd m n)
              (if (eq n 0)
                  m
                  (gcd n (mod m n))))
                  
           *)
