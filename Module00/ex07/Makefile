# Nom du programme final
EXEC=ex07

# Fichiers source
ML_FILES=ft_is_palindrome.ml test.ml

# Objets compilés
CMX_FILES=ft_is_palindrome.cmx test.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
