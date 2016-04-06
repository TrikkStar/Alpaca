#Alpaca
A statically-typed functional language developed by Noah Thederahn and Dalton McCleery.
Created for Hanover College's Programming Languages course.

##To compile:

- Navigate to `./Alpaca`
- `ocamlyacc parser.mly`
- `ocamllex lexer.mll`
- `ocamlc -c types.mli parser.mli lexer.ml parser.ml types.ml driver.ml`
- `ocamlc -o lang lexer.cmo parser.cmo types.cmo driver.cmo`
- `./lang`

You can use it interactively as above. Or you can write a "program" in any file, then run it as input to the interpreter by:

`./lang < sample_input.txt`

##Important Files
- details.md: add description and link
- semantics.md: add description and link
- sample_input.md: add description and link
