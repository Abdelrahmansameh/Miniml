FILES=	ast.ml     \
	lexer.mll  \
	parser.mly \
	printer.ml \
	main.ml \
	type.ml \
	eval.ml

CMOS=  unix.cma ast.cmo      \
	parser.cmo lexer.cmo  \
	printer.cmo eval.cmo \
	type.cmo \
	main.cmo \

exec.exe: $(FILES)
	ocamlc -c ast.ml         && \
	ocamlc -c printer.mli 	 && \
	ocamlc -c printer.ml 	 && \
	ocamlc -c eval.ml && \
	ocamlc -c type.ml && \
	ocamlyacc -v parser.mly  && \
	ocamllex  lexer.mll      && \
	ocamlc -c parser.mli     && \
	ocamlc -c parser.ml      && \
	ocamlc -c lexer.ml       && \
	ocamlc -c main.ml        && \
	ocamlc -o exec $(CMOS)
clean:
	rm -f *.cmo *.cmi *~ lexer.ml parser.ml parser.mli parser.output exec
