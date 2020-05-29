all: compile

compile:
	@ ocamlc -c Functions.ml
	@ ocamlyacc parser.mly
	@ ocamlc -c parser.mli
	@ ocamllex lexer.mll
	@ ocamlc -c lexer.ml
	@ ocamlc -c parser.ml
	@ ocamlc -c Driver.ml
	@ ocamlc -o assignment4 str.cma Functions.cmo parser.cmo lexer.cmo Driver.cmo

run:
	./assignment4 Sheet.csv 7 10 Commands

clean:
	@ rm -rf lexer.cmi
	@ rm -rf lexer.cmo
	@ rm -rf lexer.ml
	@ rm -rf parser.cmi
	@ rm -rf parser.cmo
	@ rm -rf parser.mli
	@ rm -rf parser.ml
	@ rm -rf Functions.cmi
	@ rm -rf Functions.cmo
	@ rm -rf Driver.cmi
	@ rm -rf Driver.cmo
	@ rm -rf assignment4

