exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)
exception Type of string

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
            | CallS of exprS * string list


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
            | CallC of exprC * string list

type exprT = NumT
            | BoolT
            | LetT of string * exprT
            | ListT of exprT
            | TupleT of exprT list
            | FunT of exprT * exprT
            | VarT of string

type 'a env

type value = Num of float
            | Bool of bool
            | List of value list
            | Tuple of value list
            | Clos of exprC * (value env)
            | Let of value env
            
(* Environment lookup *)

val empty : 'a env
val lookup : string -> 'a env -> 'a option
val bind :  string -> 'a -> 'a env -> 'a env

val addToFront : 'a -> 'a list -> 'a list
val isEmpty : 'a list -> bool
val getFirst : 'a list -> 'a
val getLast : 'a list -> 'a

(* typechecking*)
val typecheck : exprT env -> exprC -> exprT

(* Interpreter steps *)
val desugar : exprS -> exprC
val interp : value env -> exprC -> value
val evaluate : exprC -> exprT * value

(* result post-processing *)
val valToString : value -> string
val typToString : exprT -> string
val outputToString : exprT * value -> string

val bothToString : string * string -> string
