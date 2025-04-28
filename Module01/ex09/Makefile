# Nom du programme final
EXEC=ex09

# Fichiers source
ML_FILES=leibniz_pi.ml

# Objets compilés
CMX_FILES=leibniz_pi.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
