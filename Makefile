SRCDIR = src
COMPILER = $(SRCDIR)/simc

SRC_FILES = $(SRCDIR)/parser.mly $(SRCDIR)/lexer.mll $(SRCDIR)/sim.ml \
            $(SRCDIR)/ast.ml $(SRCDIR)/types.ml $(SRCDIR)/table.ml \
            $(SRCDIR)/semant.ml $(SRCDIR)/emitter.ml

.PHONY: all clean

all: $(COMPILER)

$(COMPILER): $(SRC_FILES)
	cd $(SRCDIR) && ocamlyacc parser.mly
	cd $(SRCDIR) && ocamllex lexer.mll
	cd $(SRCDIR) && ocamlc -c ast.ml
	cd $(SRCDIR) && ocamlc -c parser.mli
	cd $(SRCDIR) && ocamlc -c lexer.ml
	cd $(SRCDIR) && ocamlc -c parser.ml
	cd $(SRCDIR) && ocamlc -c types.ml
	cd $(SRCDIR) && ocamlc -c table.ml
	cd $(SRCDIR) && ocamlc -c semant.ml
	cd $(SRCDIR) && ocamlc -c emitter.ml
	cd $(SRCDIR) && ocamlc -c sim.ml
	cd $(SRCDIR) && ocamlc -o simc unix.cma ast.cmo lexer.cmo parser.cmo \
	                       types.cmo table.cmo semant.cmo emitter.cmo sim.cmo

clean:
	rm -f $(SRCDIR)/*.cmi $(SRCDIR)/*.cmo
	rm -f $(SRCDIR)/parser.ml $(SRCDIR)/lexer.ml $(SRCDIR)/parser.mli
	rm -f $(SRCDIR)/simc $(SRCDIR)/print_ast
	rm -rf build
