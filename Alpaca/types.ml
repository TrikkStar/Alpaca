exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)
exception Type of string

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
            | ListS of exprS list
            | TupleS of exprS list
            | LetS of string * exprS
            | FunS of string * string list * exprS
            | VarS of string * exprS
            | CallS of exprS * exprS list (*takes a func (expr) and args*)

(* You will need to add more cases here. *)
type exprC = NumC of float
            | BoolC of bool
            | IfC of exprC * exprC * exprC
            | ArithC of string * exprC * exprC
            | CompC of string * exprC * exprC
            | EqC of exprC * exprC
            | TupleC of exprC list
            | ListC of exprC list
            | LetC of string * exprC
            | FunC of string * string list * exprC
            (*| VarC of string   ---  Not needed? Desugared into a Let statement*)
            | CallC of exprC * exprC list

type exprT = NumT
            | BoolT
            | AnyT
            | ListT of exprT
            | TupleT of exprT list
            | FunT of exprT * exprT

type 'a env = (string * 'a) list

(* You will need to add more cases here. *)
type value = Num of float
            | Bool of bool
            | List of value list
            | Tuple of value list
            | Clos of exprC * (value env)
          (*| Let of value env   -- not needed *)


let empty = []


(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = match env with
  | []          -> None
  | (s,v) :: tl -> if s = str then Some v else lookup str tl


(* val bind :  string -> 'a -> 'a env -> 'a env *)
let bind str v env = (str, v) :: env

let global_ref = ref empty

let static_ref = ref empty

(*   -- LIST Constructs --   *)

(* add element to the front of a list *)
let addToFront element lst =
  match lst with
  | [] -> element :: []
  | head :: [] -> element :: head :: []
  | head :: rest -> element :: head :: rest

(* test of a list is empty *)
let isEmpty lst = (lst = empty)


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

let rec map (f, lst) =
  match lst with
  | [] -> []
  | head :: rest -> f (head) :: map (f, rest)



(*   --  function_arg binding  --   *)

let rec bind_lsts (lst_str, lst_vals, envr) =
  match (lst_str, lst_vals) with
  | ([], []) -> envr
  | (_, []) | ([], _) -> raise (Failure "Not Correct Amount of Arguments")
  | (head_str :: rest, head_vals :: rest_vals) ->
        bind head_str head_vals (bind_lsts (rest, rest_vals, envr))



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

let typeEquals a b =
  match (a, b) with
  | (NumT, NumT) -> NumT
  | (BoolT, BoolT) -> BoolT
  | _ -> raise (Type "Types do not match")

let rec listType l =
  match l with
  | head :: tail ->
    (match head with
      | NumT ->
        if List.for_all (fun (x) -> x = NumT) l
        then NumT
        else raise (Type "All elements in a list must be the same type")
      | BoolT ->
        if List.for_all (fun (x) -> x = BoolT) l
        then NumT
        else raise (Type "All elements in a list must be the same type"))
  | [] -> AnyT


(* Type-Checker *)
let rec typecheck env exp = match exp with
  | NumC n -> NumT
  | BoolC b -> BoolT
  | IfC (a, b, c) ->
    (match (typecheck env a) with
        | BoolT -> typeEquals (typecheck env b) (typecheck env c)(*these don't necessairly need to be equal*)
        | _ -> raise (Type "If-member requires Bool"))
  | ArithC (a, x, y) ->
    (match a with
      | string ->
        (match (typeEquals (typecheck env x) (typecheck env y)) with
          | NumT -> NumT
          | _ -> raise (Type "Arithmatic operations only allow Num"))
      | _ -> raise (Type "Operator not given for arithmatic operation"))
  | CompC (a, x, y) ->
    (match a with
      | string -> typeEquals (typecheck env x) (typecheck env y)
      | _ -> raise (Type "Operator not given for comparison operation"))
  | EqC (x, y) -> typeEquals (typecheck env x) (typecheck env y)
  | TupleC t -> TupleT (List.map (fun (x) -> typecheck env x) t)
  | ListC l -> ListT (listType (List.map (fun (x) -> typecheck env x) l))
  (*| FunC (str, lst, x) -> FunT (typecheck env x) (typecheck env x) ( need to figure out how to typecheck the arg list *)
  | LetC (str, e1) -> let typ_e1 = (typecheck env e1) in
                        (static_ref := (bind str typ_e1 env); typ_e1)
  | _ -> raise (Type "Unknown Type")

(* Steps/help for Let statements in details file *)


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
  | CompS (s, a, b)  -> CompC (s, desugar a, desugar b)
  | EqS (a, b)       -> EqC (desugar a, desugar b)
  | NeqS (a, b)      -> desugar (NotS (EqS (a, b)))
  | ListS lst             -> ListC (map (desugar, lst))
  | TupleS lst            -> TupleC (map (desugar, lst))
  | LetS (var, e1)        -> LetC (var, (desugar e1))
  | FunS (name, args, e1) -> FunC (name, args, (desugar e1))
  | VarS (sym, e1)        -> LetC (sym, (desugar e1))
  | CallS (func, arg_lst) -> CallC (desugar func, map (desugar, arg_lst))


(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r = match r with
  | NumC i        -> Num i
  | BoolC b       -> Bool b
  | IfC (a, b, c) ->
    (match (interp env a) with
        | Bool x  ->
            if x
            then interp env b
            else interp env c
        | _ -> raise (Interp "Error: boolean statement needed"))
  | ArithC (a, x, y) -> arithEval a (interp  env x) (interp env y)
  | CompC (a, x, y) -> compEval a (interp  env x) (interp env y)
  | EqC (x, y)       ->  eqEval (interp env x) (interp env y)
  | ListC lst        -> List (List.map (interp env) lst)
  | TupleC lst       -> Tuple (List.map (interp env) lst)
  | LetC (var, e1)         -> let eval_e1 = (interp env e1) in
                              (global_ref := (bind var eval_e1 env); eval_e1)
  | FunC _                 -> Clos (r (* FunC *), env)
  | CallC (func, arg_lst)  ->
        let funct_val = (interp env func) in              (*  lookup args for func                          *)
        let args_val  = map ((interp env), arg_lst)       (*  bind func_args with arg_vals then extend env  *)
          in (match funct_val with                        (*  interp func_body with new, extended env       *)
              | Clos (funct, envr) ->
                      (match funct with
                      | FunC (fname, arg_lst, body_expr) ->

                                let new_env = bind_lsts (arg_lst, args_val, envr) in
                                (*(interp (bind (fname, Clos (funct, envr), new_env)) body_expr)  -- recursive? *)
                                (interp new_env body_expr)
                      | _ -> raise (Interp "Error: Not Previously Defined"))
              | _ -> raise (Interp "Error: Not a Function"))


(* evaluate : exprC -> val *)
let evaluate exprC =
  let typ = typecheck (!static_ref) exprC
    in let valu = interp (!global_ref) exprC
      in (typ, valu)


(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Num i           -> string_of_float i
  | Bool b          -> string_of_bool b
  | List lst        -> "[" ^
                       (match lst with
                       | [] -> "]"
                       | head :: rest -> valToString head ^ ", " ^ valToString (List rest)
                       )
  | Tuple lst       -> "(" ^
                       (match lst with
                       | [] -> ")"
                       | head :: rest -> valToString head ^ ", " ^ valToString (Tuple rest)
                       )

let rec typToString r = match r with
  | NumT -> "Num"
  | BoolT -> "Bool"
  | ListT l -> "List: " ^ typToString l
  | TupleT t ->
    (match t with
      | head :: rest -> typToString head ^ " * " ^ typToString (TupleT rest))
  (* | LetT (str, e) -> "var: " ^ str ^ " -> " ^ typToString e *)
  | FunT (e1, e2) ->  typToString e1 ^ " -> " ^ typToString e2

let outputToString (typ, valu) = (typToString typ) ^ " " ^ (valToString valu)

let rec bothToString (type_str, val_str) =
  "(" ^ val_str ^ ", " ^ type_str ^ ")"
