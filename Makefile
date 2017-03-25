all:
	ocamlbuild -yaccflag -v -lib unix src/main.native
	mv main.native fouine
