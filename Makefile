all:
	ocamlbuild -use-menhir -tag explain -lib unix -lib str src/main.native

slides:
	pdflatex -interaction=batchmode --shell-escape slides.tex

report:
	pdflatex -interaction=batchmode --shell-escape report.tex