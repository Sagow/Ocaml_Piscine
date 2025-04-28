# Nom du programme final
EXEC=ex08

# Fichiers source
ML_FILES=ft_sum.ml

# Objets compilés
CMX_FILES=ft_sum.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
