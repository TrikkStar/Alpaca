#Alpaca
A statically-typed functional language developed by Noah Thederahn and Dalton McCleery, created for Hanover College's Programming Languages course.

##To compile:

- Navigate to `./Alpaca`
- `ocamlyacc parser.mly`
- `ocamllex lexer.mll`
- `ocamlc -c types.mli parser.mli lexer.ml parser.ml types.ml driver.ml`
- `ocamlc -o lang lexer.cmo parser.cmo types.cmo driver.cmo`
  - On Windows use `lang.exe` then run `lang`
- `./lang`

You can use it interactively as above. Or you can write a "program" in any file, then run it as input to the interpreter by:

`./lang < sample_input.txt`

##Important Files
- [details.md] (https://github.com/TrikkStar/Alpaca/blob/master/details.md): Log of significant problems encountered during development
- [semantics.md] (https://github.com/TrikkStar/Alpaca/blob/master/semantics.md): File detailing specific semantics used in Alpaca
- [sample_input.md] (https://github.com/TrikkStar/Alpaca/blob/master/sample_input.md): Sample list of programs that can be run in Alpaca
