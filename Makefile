all:
	ocamlbuild -use-ocamlfind ef.byte

clean:
	ocamlbuild -use-ocamlfind -clean
