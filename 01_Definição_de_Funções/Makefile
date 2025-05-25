# Pastas
SRC_DIR = src 								# arquivos-fonte
OUT_DIR = build								# object files e afins
BIN_DIR = bin								# onde os executáveis vão parar

# Wildcardzinho pra captar todos os arquivos-fonte
HS_FILES = $(wildcard $(SRC_DIR)/*.hs)

# Isso serve pra nomear os executáveis com o nome dos seus arquivos fonte
BINARIES = $(patsubst $(SRC_DIR)/%.hs, $(BIN_DIR)/%, $(HS_FILES))

# Regras (só o básico dessa vez)
all: $(BINARIES)

$(BIN_DIR)/%: $(SRC_DIR)/%.hs
	mkdir -p $(OUT_DIR) $(BIN_DIR)
	ghc -outputdir $(OUT_DIR) -o $@ $<

clean:
	rm -rf $(OUT_DIR) $(BIN_DIR)

re: clean all

.PHONY: all clean re
