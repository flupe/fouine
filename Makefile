all:
	ocamlbuild -use-menhir -tag explain -lib unix -lib str src/main.native > /dev/null

slides:
	pdflatex -interaction=batchmode --shell-escape slides.tex

report:
	pdflatex -interaction=batchmode --shell-escape report.tex