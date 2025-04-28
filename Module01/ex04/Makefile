# Nom du programme final
EXEC=ex04

# Fichiers source
ML_FILES=fibonacci.ml

# Objets compilés
CMX_FILES=fibonacci.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
