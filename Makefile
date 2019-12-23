MODULES=command grid interface state npc savefile account authors 
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=interface.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build 
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)


play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip 2048.zip *.ml* _tags *.txt* Makefile .merlin .ocamlinit *.json

clean:
	ocamlbuild -clean


docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)