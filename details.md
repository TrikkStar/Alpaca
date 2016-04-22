**Log of problems encountered and solutions used for said problems:**

- VARIABLE and LET tokens were creating shift/reduce errors due to not being marked `%nonassoc`

- Typechecker needed to be implemented, this involved modifying the driver so that evaluate took both a type and a string to be printed out.

- Functions were only interpreting to a Closure, containing a string list of arguments and the environment the function was created under. Call is the interp that has to bind it's value 	  list of arguments to the Closure's string list of args then interp the expression in the body of the Function with the updated arguments. Not the other way around as I thought
  originally.

- Instead of Comments being treated as a Type Comment, they are instead treated as whitespace that is, in turn, ignored.

- Implementing the Typechecker before the additional language constructs made for myriad issues when trying to debug it. Especially since it required commenting out large portions of in-development code and causing conflicts with what had needed to be looked at.




- Let statements:

1. First you need to change your parser so that the “Let” is actually matched in the “headEx” section. That’s the one for top-level assignments, which I am guessing is what you want (if it is not, then you’ll need to tell me a bit more about what behavior you want from your let statements).

2. Then you need to create a ref to an environment, call it global_dyn_ref or something like that. It starts with an empty environment.

3. You need to change the empty environment you provide to the interp call from evaluate to instead provide the environment that is contained in that ref (you’ll need to recall how to dereference).

4. In your interpretation of “let” you need to first compute the corresponding value, via interp. Then you need to extend the environment via bind, and place that newly created environment into the reference. Finally you need to return the value.

5. This way every time you do the next evaluate it finds the updated environment.

6. You need to repeat steps 2-4 for type checking (need a ref holding a static global environment, need to provide that environment when you call “tc” from evaluate, and need to update that ref’s contents when you type check a “let".