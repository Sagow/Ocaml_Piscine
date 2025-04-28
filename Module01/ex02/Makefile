# Nom du programme final
EXEC=ex02

# Fichiers source
ML_FILES=ackermann.ml

# Objets compilés
CMX_FILES=ackermann.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
