BIN=elf_gen
SRCS=main.ml


${BIN}: ${SRCS}
	ocamlfind ocamlopt -package hex -package str -package unix -linkpkg -o ${BIN} ${SRCS}

merlin:
	./merlin_init.sh | grep hex >> .merlin

clean:
	rm -rf ${BIN} *.o *.cmi *.cmx out

