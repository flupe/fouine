all:
	ocamlbuild -use-menhir -lib unix src/main.native
