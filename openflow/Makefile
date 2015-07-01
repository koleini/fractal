PACKAGES=-package lwt.syntax,lwt,lwt.unix,cmdliner,ipaddr,str,rpclib,openflow
INCLUDE=
OPT=-linkpkg -g 
OCAMLOPT=ocamlopt -w A-4-44
FILES=client.ml
PWD=$(shell pwd)
SRC=$(PWD)
BIN=$(PWD)/bin
CLNT=mirage_client
INSTALLDIR=/usr/local/bin

all: $(BIN)/client

$(BIN)/client: $(SRC)/client.ml
	mkdir -p $(BIN)
	cd $(SRC) ; eval `opam config env` ; ocamlfind $(OCAMLOPT) $(INCLUDE) $(PACKAGES) $(OPT) $(FILES) -o $(BIN)/client -syntax camlp4o -thread

#	cd $(SRC)/$(CLNT) ;	eval `opam config env` ; $(MAKE)

clean:
	cd $(SRC) ; rm -f client client.cmx client.cmi client.o 
	cd $(SRC) ; rm -f *~ tags
	cd $(BIN) ; rm -f client
	cd $(SRC)/$(CLNT) ; rm -f main.native mir-client
