.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

plinko:
	OCAMLRUNPARAM=b dune exec bin/plinko.exe
	
bounce:
	OCAMLRUNPARAM=b dune exec bin/bounce.exe

springs:
	OCAMLRUNPARAM=b dune exec bin/springs.exe

zip:
	rm -f physSim.zip
	zip -r physSim.zip . -x@exclude.lst

clean:
	dune clean
	rm -f adventure.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

bisect:
	dune exec --instrument-with bisect_ppx test/main.exe
	bisect-ppx-report html
	open _coverage/index.html
	rm bisect*.coverage

rmCov:
	rm _coverage

