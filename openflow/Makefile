PACKAGES=-package lwt.syntax,lwt,lwt.unix,cmdliner,ipaddr,str,jsonm,openflow
INCLUDE=
OPT=-linkpkg -g 
OCAMLOPT=ocamlopt -w A-4-44
FILES=\
	  of13controller.mli \
	  of13controller.ml \
	  dbController.mli \
	  dbController.ml \
	  naasLoad.mli \
	  naasLoad.ml \
	  client.ml
PWD=$(shell pwd)
SRC=$(PWD)
BIN=$(PWD)/bin
CLNT=mirage_client
INSTALLDIR=/usr/local/bin

all: $(BIN)/client

$(BIN)/client:  $(SRC)/naasLoad.ml $(SRC)/naasLoad.mli $(SRC)/of13controller.mli $(SRC)/of13controller.ml $(SRC)/dbController.ml $(SRC)/dbController.mli $(SRC)/client.ml

	mkdir -p $(BIN)
	cd $(SRC) ; eval `opam config env` ; ocamlfind $(OCAMLOPT) $(INCLUDE) $(PACKAGES) $(OPT) $(FILES) -o $(BIN)/client -syntax camlp4o -thread

#	cd $(SRC)/$(CLNT) ;	eval `opam config env` ; $(MAKE)

clean:
	cd $(SRC) ; rm -f client client.cmx client.cmi client.o of13controller.cmx of13controller.cmi of13controller.o naasLoad.cmx naasLoad.cmi naasLoad.o
	cd $(SRC) ; rm -f *~ tags
	cd $(BIN) ; rm -f client
	cd $(SRC)/$(CLNT) ; rm -f main.native mir-client
