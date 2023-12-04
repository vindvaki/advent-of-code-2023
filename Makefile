# builds a binary called run-tests
run-tests: *.lisp *.asd
	sbcl \
		--noinform \
		--lose-on-corruption \
		--end-runtime-options \
		--eval '(ql:quickload :advent-of-code-2023-tests)' \
		--eval '(asdf:make :advent-of-code-2023-tests)' \
		--quit \
		--non-interactive

# runs tests dynamically from source
.PHONY: test
test: *.lisp *.asd *.input
	sbcl \
		--noinform \
		--lose-on-corruption \
		--end-runtime-options \
		--eval '(ql:quickload :advent-of-code-2023-tests)' \
		--eval '(asdf:test-system :advent-of-code-2023)' \
		--quit \
		--non-interactive

dependency-resolver: dependency-resolver.lisp dependency-resolver.asd
	sbcl \
		--noinform \
		--lose-on-corruption \
		--end-runtime-options \
		--eval '(ql:quickload :dependency-resolver)' \
		--eval '(asdf:make :dependency-resolver)' \
		--quit \
		--non-interactive

all-dependencies.txt: dependency-resolver
	./dependency-resolver > all-dependencies.txt
