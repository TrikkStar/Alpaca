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
            | ListS of list
            | TupleS of exprS list

(* You will need to add more cases here. *)
type exprC = NumC of float
            | BoolC of bool
            | IfC of exprC * exprC * exprC
            | ArithC of string * exprC * exprC
            | CompC of string * exprC * exprC
            | EqC of exprC * exprC
            | ListC of list
            | TupleC of exprC list

type exprT = NumT of float
            | BoolT of bool
            | ListT of list

(* You will need to add more cases here. *)
type value = Num of float
            | Bool of bool
            | List of list

type 'a env = (string * 'a) list
let empty = []


(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = match env with
  | []          -> None
  | (s,v) :: tl -> if s = str then Some v else lookup str tl


(* val bind :  string -> 'a -> 'a env -> 'a env *)
let bind str v env = (str, v) :: env


(*   -- LIST Constructs --   *)

(* add element to the front of a list *)
let addToFront element lst =
  match lst with
  | [] -> element :: []
  | head :: rest -> element :: head :: rest

(* test of a list is empty *)
let isEmpty lst = 
  match lst with
  | [] -> true
  | _ -> false


(* access the head of a list *)
let getFirst lst =
  match lst with
  | [] -> raise (Failure ("List is empty"))
  | head :: rest -> head


(* access the tail of a list *)
let rec getLast lst =
  match lst with 
  | [] -> raise (Failure ("List is empty"))
  | tail :: [] -> tail
  | head :: rest -> getLast rest

(*    --              --      *)


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


(* Type-Checker *)
let rec typecheck env exp = match exp with
  | NumC i -> NumT i
  | BoolC b -> BoolT b
  | ListC l -> ListT l
  | IfC (a, b, c) -> 
    (match (typecheck env a) with
        | BoolT -> 
          (match ((typecheck env b), (typecheck env c)) with
            | (NumT x, NumT y) -> NumT x
            | (BoolT x, BoolT y) -> BoolT x
            | (ListT x, ListT y) -> ListT x
            | _ -> (Failure "type Error"))
        | _ -> raise (Failure "type Error"))
  | ArithC (a, x, y) ->
  | CompC (a, x, y) ->
  | EqC (x, y) ->
    (match ((typecheck env x), (typecheck env y)) with
            | (NumT a, NumT b) -> NumT a
            | (BoolT a, BoolT b) -> BoolT a
            | (ListT a, ListT b) -> ListT a
            | _ -> (Failure "type Error"))


(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = match exprS with
  | NumS i        -> NumC i
  | BoolS b       -> BoolC b
  | ListS lst     -> ListC lst
  | IfS (a, b, c) -> IfC (desugar a, desugar b, desugar c)
  | NotS e        -> IfC (desugar e, BoolC false, BoolC true)
  | OrS (e1, e2)  -> IfC (desugar e1, BoolC true, IfC (desugar e2, BoolC true, BoolC false))
  | AndS (e1, e2) -> IfC (desugar e1, IfC (desugar e2, BoolC true, BoolC false), BoolC false)
  | ArithS (s, a, b) -> ArithC (s, desugar a, desugar b)
  | CompS (s, a, b) -> CompC (s, desugar a, desugar b)
  | EqS (a, b)    -> EqC (desugar a, desugar b)
  | NeqS (a, b)   -> desugar (NotS (EqS (a, b)))
  | TupleS lst    -> desugar (TupleC lst)

(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r = match r with
  | NumC i        -> Num i
  | BoolC b       -> Bool b
  | ListC lst     -> List lst
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
  | TupleC lst      -> match lst with
                       | [] -> []
                       | head :: rest -> (interp env head) @ (interp env rest) 


(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []




(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Num i           -> string_of_float i
  | Bool b          -> string_of_bool b
(*| List lst        -> *)
