# Nom du programme final
EXEC=ex03

# Fichiers source
ML_FILES=sequence.ml test.ml

# Objets compilés
CMX_FILES=sequence.cmx test.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
