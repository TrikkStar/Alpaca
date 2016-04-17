**Log of problems encountered and solutions used for said problems:**

- VARIABLE and LET tokens were creating shift/reduce errors due to not being marked `%nonassoc`

- Typechecker needed to be implemented, this involved modifying the driver so that evaluate took both a type and a string to be printed out. 
