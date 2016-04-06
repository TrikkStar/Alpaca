exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
type exprS = NumS of float
            | BoolS of bool
            | IfS of exprS * exprS * exprS
            | OrS of exprS * exprS
            | AndS of exprS * exprS
            | NotS of exprS
            | ArithS of string * exprS * exprS
            | CompS of string * exprS * exprS
            | EqS of exprS * exprS
            | NeqS of exprS * exprS

(* You will need to add more cases here. *)
type exprC = NumC of float
            | BoolC of bool
            | IfC of exprC * exprC * exprC
            | ArithC of string * exprC * exprC
            | CompC of string * exprC * exprC
            | EqC of exprC * exprC

(* You will need to add more cases here. *)
type value = Num of float
            | Bool of bool

type 'a env = (string * 'a) list
let empty = []

(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = match env with
  | []          -> None
  | (s,v) :: tl -> if s = str then Some v else lookup str tl
(* val bind :  string -> 'a -> 'a env -> 'a env *)
let bind str v env = (str, v) :: env


(*
   HELPER METHODS
   You may be asked to add methods here. You may also choose to add your own
   helper methods here.
*)

let arithEval str x y =
  match (x, y) with
  | (Num a, Num b) ->
    (match str with
        | "+" -> Num (a +. b)
        | "-" -> Num (a -. b)
        | "*" -> Num (a *. b)
        | "/" -> if b = 0.0
                then raise (Interp "Error: invalid denominator")
                else Num (a /. b)
        | _ -> raise (Interp "Error: invalid symbol"))
  | _ -> raise (Interp "Error: invalid type")

let compEval str x y =
  match (x, y) with
  | (Num a, Num b) ->
    (match str with
        | ">" -> Bool (a > b)
        | ">=" -> Bool (a >= b)
        | "<" -> Bool (a < b)
        | "<=" -> Bool (a <= b)
        | _ -> raise (Interp "Error: invalid operator"))
  | _ -> raise (Interp "Error: invalid type")

let eqEval x y =
  match (x, y) with
  | (Bool a, Bool b) -> Bool (a = b)
  | (Num a, Num b) -> Bool (a = b)
  | _ -> Bool false

(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = match exprS with
  | NumS i        -> NumC i
  | BoolS b       -> BoolC b
  | IfS (a, b, c) -> IfC (desugar a, desugar b, desugar c)
  | NotS e        -> IfC (desugar e, BoolC false, BoolC true)
  | OrS (e1, e2)  -> IfC (desugar e1, BoolC true, IfC (desugar e2, BoolC true, BoolC false))
  | AndS (e1, e2) -> IfC (desugar e1, IfC (desugar e2, BoolC true, BoolC false), BoolC false)
  | ArithS (s, a, b) -> ArithC (s, desugar a, desugar b)
  | CompS (s, a, b) -> CompC (s, desugar a, desugar b)
  | EqS (a, b)    -> EqC (desugar a, desugar b)
  | NeqS (a, b)   -> desugar (NotS (EqS (a, b)))

(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r = match r with
  | NumC i        -> Num i
  | BoolC b       -> Bool b
  | IfC (a, b, c) ->
    (match (interp env a) with
        | Bool x ->
            if x
            then interp env b
            else interp env c
        | _ -> raise (Interp "Error: boolean statement needed"))
  | ArithC (a, x, y) -> arithEval a (interp  env x) (interp env y)
  | CompC (a, x, y) -> compEval a (interp  env x) (interp env y)
  | EqC (x, y) ->  eqEval (interp env x) (interp env y)

(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []




(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Num i           -> string_of_float i
  | Bool b          -> string_of_bool b
