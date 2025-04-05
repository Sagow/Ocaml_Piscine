# Nom du programme final
EXEC=ex09

# Fichiers source
ML_FILES=ft_print_comb2.ml test.ml

# Objets compilés
CMX_FILES=ft_print_comb2.cmx test.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
