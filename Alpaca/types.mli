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
            | FunS of string list * exprS
            | VarS of string

type exprC = NumC of float
            | BoolC of bool
            | IfC of exprC * exprC * exprC
            | ArithC of string * exprC * exprC
            | CompC of string * exprC * exprC
            | EqC of exprC * exprC
            | ListC of exprC list
            | TupleC of exprC list
            | LetC of string * exprC
            | FunC of string list * exprC
            | VarC of string

type exprT = NumT
            | BoolT
            | ListT of exprT list
            | TupleT of exprT list
            | LetT of string * exprT
            | FunT of exprT * exprT
            | VarT of string

type value = Num of float
            | Bool of bool
            | List of value list
            | Tuple of value list

(* Environment lookup *)
type 'a env
val empty : 'a env
val lookup : string -> 'a env -> 'a option
val bind :  string -> 'a -> 'a env -> 'a env

val addToFront : 'a -> 'a list -> 'a list
val isEmpty : 'a list -> bool
val getFirst : 'a list -> 'a
val getLast : 'a list -> 'a

(* typechecking*)
val typecheck : exprC env -> exprT

(* Interpreter steps *)
val desugar : exprS -> exprC
val interp : value env -> exprC -> value
val evaluate : exprC -> exprT * value

(* result post-processing *)
val valToString : value -> string
val typToString : exprT -> string
val outputToString : exprT * value -> string

val bothToString : string -> string -> string
