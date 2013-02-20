OCAMLBUILD=ocamlbuild -use-ocamlfind -build-dir build -I src

.PHONY: doc

all: client.native meta.native game.native
	mkdir -p bin
	ln -sf ../build/src/client.native bin/client
	ln -sf ../build/src/meta.native bin/meta
	ln -sf ../build/src/game.native bin/game

debug: client.d.byte meta.d.byte game.d.byte
	mkdir -p debug
	ln -sf ../build/src/client.d.byte debug/client
	ln -sf ../build/src/meta.d.byte debug/meta
	ln -sf ../build/src/game.d.byte debug/game

doc:
	$(OCAMLBUILD) -docflag -stars src/bombercat.docdir/index.html
	ln -sf build/src/bombercat.docdir doc

clean:
	$(OCAMLBUILD) -clean
	rm -rf bin
	rm -rf doc
	rm -rf debug

%.native:
	$(OCAMLBUILD) $@

%.d.byte:
	$(OCAMLBUILD) -cflag -ppopt -cflag -lwt-debug $@
