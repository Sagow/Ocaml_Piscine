# Nom du programme final
EXEC=ex04

# Fichiers source
ML_FILES=nucleotides.ml test.ml

# Objets compilés
CMX_FILES=nucleotides.cmx test.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
