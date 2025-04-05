# Nom du programme final
EXEC=ex01

# Fichiers source
ML_FILES=ft_countdown.ml test.ml

# Objets compilés
CMX_FILES=ft_countdown.cmx test.cmx

# Compilation native
all: $(EXEC)

$(EXEC): $(CMX_FILES)
	ocamlopt -o $(EXEC) $(CMX_FILES)

%.cmx: %.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o $(EXEC)
