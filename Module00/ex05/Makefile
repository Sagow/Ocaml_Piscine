# Nom du programme final
EXEC=ex05

# Fichiers source
ML_FILES=ft_print_rev.ml test.ml

# Objets compilés
CMX_FILES=ft_print_rev.cmx test.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
