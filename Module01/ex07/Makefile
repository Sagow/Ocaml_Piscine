# Nom du programme final
EXEC=ex07

# Fichiers source
ML_FILES=converges.ml

# Objets compilés
CMX_FILES=converges.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
