# Nom du programme final
EXEC=ex08

# Fichiers source
ML_FILES=life.ml test.ml

# Objets compilés
CMX_FILES=life.cmx test.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
