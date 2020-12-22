all: dlpag

.PHONY: clean dlpag

clean:
	rm -f *~ -r __pycache__
	ocamlbuild -clean

dlpag:
	ocamlbuild $(OPTIONS) -no-links src/Main.native
	cp _build/src/Main.native dlpag

OPTIONS = -j 4 -use-menhir -use-ocamlfind -yaccflags --table,--explain -pkgs menhirLib,str,unix,tsort

errorfile:
	menhir --list-errors src/Parser.mly > src/ParserMessages.messages
