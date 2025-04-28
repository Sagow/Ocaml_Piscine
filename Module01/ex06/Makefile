# Nom du programme final
EXEC=ex06

# Fichiers source
ML_FILES=iter.ml

# Objets compilés
CMX_FILES=iter.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
