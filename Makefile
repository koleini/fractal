PACKAGES=-package lwt.syntax,lwt,dns.lwt,libvirt,cmdliner,ezxmlm,ipaddr,str,conduit,conduit.lwt-unix,rpclib,xen-api-client,xen-api-client.lwt
INCLUDE=
OPT=-linkpkg -g 
OCAMLOPT=ocamlopt -w A-4-44
FILES=backends.mli pool.ml libvirt_backend.ml xapi_backend.ml synjitsu.mli synjitsu.ml jitsu.mli jitsu.ml main.ml
PWD=$(shell pwd)
SRC=$(PWD)/src
BIN=$(PWD)/bin
CLNT=mirage_client
INSTALLDIR=/usr/local/bin

all: $(BIN)/jitsu

$(BIN)/jitsu: $(SRC)/jitsu.ml $(SRC)/main.ml $(SRC)/synjitsu.mli $(SRC)/synjitsu.ml $(SRC)/jitsu.mli $(SRC)/libvirt_backend.ml $(SRC)/xapi_backend.ml $(SRC)/backends.mli $(SRC)/pool.ml $(SRC)/$(CLNT)/client.ml
	mkdir -p $(BIN)
	cd $(SRC)/$(CLNT) ;	$(MAKE)
	cd $(SRC) ; ocamlfind $(OCAMLOPT) $(INCLUDE) $(PACKAGES) $(OPT) $(FILES) -o $(BIN)/jitsu -syntax camlp4o

install: $(BIN)/jitsu
	@echo "Installing jitsu in $(INSTALLDIR)..."
	install -s $(BIN)/jitsu $(INSTALLDIR)/jitsu

clean:
	cd $(SRC) ; rm -f jitsu jitsu.cmx jitsu.cmi jitsu.o 
	cd $(SRC) ; rm -f main.o main.cmx main.cmi 
	cd $(SRC) ; rm -f backends.o backends.cmx backends.cmi 
	cd $(SRC) ; rm -f libvirt_backend.o libvirt_backend.cmx libvirt_backend.cmi 
	cd $(SRC) ; rm -f xapi_backend.o xapi_backend.cmx xapi_backend.cmi 
	cd $(SRC) ; rm -f synjitsu.o synjitsu.cmx synjitsu.cmi
	cd $(SRC) ; rm -f *~ tags
	cd $(BIN) ; rm -f jitsu
	cd $(SRC)/$(CLNT) ; rm -f main.native mir-client
