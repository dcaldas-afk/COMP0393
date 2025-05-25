# Diretórios
SRC_DIR = src
OUT_DIR = build
BIN_DIR = bin

# Arquivos-fonte Haskell
HS_FILES = $(wildcard $(SRC_DIR)/*.hs)

# Nomes dos executáveis (mesmo nome do arquivo .hs)
BINARIES = $(patsubst $(SRC_DIR)/%.hs, $(BIN_DIR)/%, $(HS_FILES))

# Regra padrão: compilar tudo
all: $(BINARIES)

# Como compilar cada executável
$(BIN_DIR)/%: $(SRC_DIR)/%.hs
	mkdir -p $(OUT_DIR) $(BIN_DIR)
	ghc -outputdir $(OUT_DIR) -o $@ $<

# Limpar binários e arquivos intermediários
clean:
	rm -rf $(OUT_DIR) $(BIN_DIR)

# Recompilar do zero
re: clean all

.PHONY: all clean re
