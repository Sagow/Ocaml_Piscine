# Nom du programme final
EXEC=ex01

# Fichiers source
ML_FILES=crossover.ml test.ml

# Objets compilés
CMX_FILES=crossover.cmx test.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
