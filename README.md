# F28PL Interpreter (part 1)

While the interpreter is not yet finished, you may want to try it out
on some examples. 

To compile the sources and load them into utop, do the following in
the terminal:

```
$ ocamlc Syntax.ml
$ ocamlc Tokenize.ml
$ ocamlc Parse.ml
$ ocamlc Interp.ml
$ utop
# #use "init.mlt"
```

The `init.mlt` loads the compiled files to the OCaml interpreter, so
that you can now `open` the particular modules. Note that this is
likely not to work just yet in VSCode just yet, at least not out of
the box, but we will get there in the updated version tomorrow.
