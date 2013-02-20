OCAMLBUILD=ocamlbuild -use-ocamlfind -build-dir build -I src

all: client.native meta.native game.native
	mkdir -p bin
	ln -s ../build/src/client.native bin/client
	ln -s ../build/src/meta.native bin/meta
	ln -s ../build/src/game.native bin/game

debug: client.d.byte meta.d.byte game.d.byte

doc:
	$(OCAMLBUILD) -docflag -stars src/bombercat.docdir/index.html
	ln -s build/src/bombercat.docdir doc

clean:
	$(OCAMLBUILD) -clean
	rm -rf bin
	rm -rf doc

%.native:
	$(OCAMLBUILD) $@

%.d.byte:
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug $@
