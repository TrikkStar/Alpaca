open Types

(* You can test expressions of type resultS or resultC and how they are evaluated *)
(* These will only work once you have compiled types.ml *)

(* This is the one kind of test you can write. *)
let t0a = evaluate (NumC 2.3) = Num 2.3

(* You can also use interp directly to specify a custom environment. *)
let t0b = let env1 = bind "x" (Num 3.1) empty
          in interp env1 (NumC 2.3) = Num 2.3

(* You can also test desugar to make sure it behaves properly. *)
let t0c = desugar (NumS 2.3) = NumC 2.3

(* Or you can combine with evaluate to get to the final value. *)
let t0d = evaluate (desugar (NumS 2.3)) = Num 2.3

let t1a = evaluate (BoolC false) = Bool false
let t1b = evaluate (BoolC true) = Bool true
let t1c = desugar (BoolS false) = BoolC false
let t1d = desugar (BoolS true) = BoolC true

let t2a = evaluate (IfC (BoolC true, NumC 2.3, NumC 4.3)) = Num 2.3
let t2b = evaluate (IfC (IfC (BoolC false, BoolC true, BoolC false), NumC 2.7, NumC 4.3)) = Num 4.3
let t2c = evaluate (desugar (NotS (BoolS true))) = Bool false
let t2d = evaluate (desugar (OrS (BoolS false, BoolS true))) = Bool true
let t2e = evaluate (desugar (AndS (BoolS false, BoolS true))) = Bool false

let t3a = evaluate (ArithC ("+", NumC 2.0, NumC 4.0)) = Num 6.0
let t3b = evaluate (desugar (ArithS ("*", NumS 7.0, NumS 2.0))) = Num 14.0
let t3c = evaluate (desugar (ArithS ("/", NumS 37.0, NumS 4.0))) = Num 9.25
let t3d = evaluate (desugar (ArithS ("-", NumS 3.0, NumS 12.0))) = Num (-9.0)

let t4a = evaluate (desugar (CompS (">", NumS 7.0, NumS 2.0))) = Bool true
let t4b = evaluate (desugar (CompS (">=", NumS 7.0, NumS 42.0))) = Bool false
let t4c = evaluate (desugar (CompS ("<", NumS 7.0, NumS 2.0))) = Bool false
let t4d = evaluate (desugar (CompS ("<=", NumS 2.0, NumS 2.0))) = Bool true

let t5a = evaluate (desugar (EqS (NumS 7.0, NumS 2.0))) = Bool false
let t5b = evaluate (desugar (EqS (BoolS false, BoolS false))) = Bool true
let t5c = evaluate (desugar (NeqS (NumS 7.0, NumS 2.0))) = Bool true
let t5d = evaluate (desugar (NeqS (BoolS true, BoolS true))) = Bool false
