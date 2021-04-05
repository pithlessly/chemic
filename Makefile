ocaml_exec := _build/default/main.bc
ocaml_files := main.ml src/*

runtime := tests/chemic.o

test_driver := tests/main.py
test_file := tests/test.md

.PHONY: test
test: $(ocaml_exec) $(runtime) tests/main.ml tests/tests.md
	cd tests && ocaml unix.cma main.ml tests.md

$(runtime): runtime/src/chemic.c runtime/include/chemic.h
	cc -fsanitize=undefined -Iruntime/include $< -c -o $@

$(ocaml_exec): $(ocaml_files)
	dune build main.bc
	@touch $@ # needed so make knows the file is new
