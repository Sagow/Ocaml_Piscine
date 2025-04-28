# Nom du programme final
EXEC=ex06

# Fichiers source
ML_FILES=rna.ml test.ml

# Objets compilés
CMX_FILES=rna.cmx test.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
