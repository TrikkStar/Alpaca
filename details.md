**Log of problems encountered and solutions used for said problems:**

- VARIABLE and LET tokens were creating shift/reduce errors due to not being marked `%nonassoc`

- Typechecker needed to be implemented, this involved modifying the driver so that evaluate took both a type and a string to be printed out. 

- Functions were only interperating to a Closure, containing a string list of arguments and the enviroment the function was created under. Call is the interp that has to bind it's value 	  list of arguments to the Closure's string list of args then interp the expression in the body of the Function with the updated arguments. Not the other way around as I thought
  originally. 

- Instead of Comments being treated as a Type Comment, they are instead treated as whitespace that is, in turn, ignored.

- 