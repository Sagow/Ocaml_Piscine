# Nom du programme final
EXEC=ex01

# Fichiers source
ML_FILES=Value.ml main.ml

# Objets compilés
CMX_FILES=Value.cmx main.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
