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
            | TupleS of exprS list
            | LetS of string * exprS * exprS
            | FunS of exprS * exprS
            (*| VarS of *)

type exprC = NumC of float
            | BoolC of bool
            | IfC of exprC * exprC * exprC
            | ArithC of string * exprC * exprC
            | CompC of string * exprC * exprC
            | EqC of exprC * exprC
            | TupleC of exprC list
            | LetC of string * exprC * exprC
            | FunC of exprC * exprC
            (*| VarC of *)

type exprT = NumT
            | BoolT
            | ListT of exprT list
            | TupleT of exprT list
            | LetT of string * exprT * exprT
            | FunT of exprT * exprT
            (*| VarT of *)

type value = Num of float
            | Bool of bool
            | List of list
            | Tuple of list

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
val typecheck : exprC -> exprT

(* Interpreter steps *)
val desugar : exprS -> exprC
val interp : value env -> exprC -> value
val evaluate : exprC -> value

(* result post-processing *)
val valToString : value -> string
