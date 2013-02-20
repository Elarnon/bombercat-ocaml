OCAMLBUILD=ocamlbuild -use-ocamlfind

all: client.native meta.native game.native

debug: client.d.byte meta.d.byte game.d.byte

doc:
	$(OCAMLBUILD) -docflag -stars bombercat.docdir/index.html

clean:
	$(OCAMLBUILD) -clean

%.native: %.ml
	$(OCAMLBUILD) $@

%.d.byte: %.ml
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug $@
